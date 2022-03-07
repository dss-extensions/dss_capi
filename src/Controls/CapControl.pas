unit CapControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//  A CapControl is a control element that is connected to a terminal of another
//  circuit element and controls a capacitor.  The control is usually placed in the
//  terminal of a line or transformer, although a voltage control device could be placed
//  in the terminal of the capacitor it controls
//
//  A CapControl is defined by a New command:
//
//  New CapControl.Name=myname Element=devclass.name terminal=[ 1|2|...] Capacitor = name
//
//  Capacitor to be controlled must already exist.

interface

uses
    Classes,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    Bus,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    Capacitor,
    utilities,
    CapControlVars,
    CapUserControl;

type
{$SCOPEDENUMS ON}
    TCapControlProp = (
        INVALID = 0,
        element = 1,
        terminal = 2,
        capacitor = 3,
        typ = 4,
        PTratio = 5,
        CTratio = 6,
        ONsetting = 7,
        OFFsetting = 8,
        Delay = 9,
        VoltOverride = 10,
        Vmax = 11,
        Vmin = 12,
        DelayOFF = 13,
        DeadTime = 14,
        CTPhase = 15,
        PTPhase = 16,
        VBus = 17,
        EventLog = 18,
        UserModel = 19,
        UserData = 20,
        pctMinkvar = 21,
        Reset = 22
    );
{$SCOPEDENUMS OFF}

    TCapControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TCapControlObj = class(TControlElem)
    PUBLIC
        procedure Set_Enabled(Value: Boolean); OVERRIDE;
    PRIVATE
        ControlType: ECapControlType;
        ControlVars: TCapControlVars;
        ControlledCapacitor: TCapacitorObj;
        cBuffer: pComplexArray;    // Complexarray buffer

        IsUserModel: Boolean;
        UserModel: TCapUserControl;
        UserModelNameStr, UserModelEditStr: String;

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
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

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
        property UseVoltageOverride: LongBool READ ControlVars.Voverride;
        property DeadTimeVal: Double READ ControlVars.DeadTime;
        property PTPhase: Integer READ ControlVars.FPTPhase;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type 
    TObj = TCapControlObj;
    TProp = TCapControlProp;
const
    NumPropsThisClass = Ord(High(TProp));
    AVGPHASES = -1;
    MAXPHASE = -2;
    MINPHASE = -3;
var
    PropInfo: Pointer = NIL;
    TypeEnum: TDSSEnum;

constructor TCapControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        TypeEnum := TDSSEnum.Create('CapControl: Type', True, 1, 1, 
            ['Current', 'Voltage', 'kvar', 'Time', 'PowerFactor'{'UserControl'}], 
            [ord(CURRENTCONTROL), ord(VOLTAGECONTROL), ord(KVARCONTROL), ord(TIMECONTROL), ord(PFCONTROL) {, ord(USERCONTROL)}]);
    end;
    inherited Create(dssContext, CAP_CONTROL, 'CapControl');
end;

destructor TCapControl.Destroy;
begin
    inherited Destroy;
end;

procedure DoReset(Obj: TObj);
begin
    // force a reset
    Obj.Reset;
    //PropertyValue[22] := 'n'; // so it gets reported properly
end;

procedure TCapControl.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    NumProperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, False);

    // object references
    PropertyType[ord(TProp.capacitor)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.capacitor)] := ptruint(@obj.FControlledElement);
    PropertyOffset2[ord(TProp.capacitor)] := ptruint(DSS.CapacitorClass);
    PropertyWriteFunction[ord(TProp.capacitor)] := @SetControlledElement;
    PropertyFlags[ord(TProp.capacitor)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.CheckForVar]; // will automatically substitute @var value

    PropertyType[ord(TProp.element)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.element)] := ptruint(@obj.FMonitoredElement);
    PropertyOffset2[ord(TProp.element)] := 0;
    PropertyWriteFunction[ord(TProp.element)] := @SetMonitoredElement;
    PropertyFlags[ord(TProp.element)] := [TPropertyFlag.WriteByFunction];
    //PropertyFlags[ord(TProp.element)] := [TPropertyFlag.CheckForVar]; // not required for general cktelements

    // enum properties
    PropertyType[ord(TProp.typ)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.typ)] := ptruint(@obj.ControlType);
    PropertyOffset2[ord(TProp.typ)] := PtrInt(TypeEnum);

    PropertyType[ord(TProp.PTphase)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.PTphase)] := ptruint(@obj.ControlVars.FPTPhase);
    PropertyOffset2[ord(TProp.PTphase)] := PtrInt(DSS.MonPhaseEnum);

    PropertyType[ord(TProp.CTPhase)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.CTPhase)] := ptruint(@obj.ControlVars.FCTPhase);
    PropertyOffset2[ord(TProp.CTPhase)] := PtrInt(DSS.MonPhaseEnum);

    // string properties
    PropertyType[ord(TProp.VBus)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.VBus)] := ptruint(@obj.ControlVars.VOverrideBusName);

    PropertyType[ord(TProp.UserModel)] := TPropertyType.StringProperty;
    PropertyType[ord(TProp.UserData)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.UserModel)] := ptruint(@obj.UserModelNameStr);
    PropertyOffset[ord(TProp.UserData)] := ptruint(@obj.UserModelEditStr);

    // boolean properties
    PropertyType[ord(TProp.VoltOverride)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.VoltOverride)] := ptruint(@obj.ControlVars.Voverride);
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);

    // integer properties
    PropertyType[ord(TProp.terminal)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.terminal)] := ptruint(@obj.ElementTerminal);

    // double properties (default type)
    PropertyOffset[ord(TProp.PTratio)] := ptruint(@obj.ControlVars.PTRatio);
    PropertyOffset[ord(TProp.CTratio)] := ptruint(@obj.ControlVars.CTRatio);
    PropertyOffset[ord(TProp.ONsetting)] := ptruint(@obj.ControlVars.ON_Value);
    PropertyOffset[ord(TProp.OFFsetting)] := ptruint(@obj.ControlVars.OFF_Value);
    PropertyOffset[ord(TProp.Delay)] := ptruint(@obj.ControlVars.ONDelay);
    PropertyOffset[ord(TProp.Vmax)] := ptruint(@obj.ControlVars.Vmax);
    PropertyOffset[ord(TProp.Vmin)] := ptruint(@obj.ControlVars.Vmin);
    PropertyOffset[ord(TProp.DelayOFF)] := ptruint(@obj.ControlVars.OFFDelay);
    PropertyOffset[ord(TProp.DeadTime)] := ptruint(@obj.ControlVars.DeadTime);
    PropertyOffset[ord(TProp.pctMinkvar)] := ptruint(@obj.FpctMinKvar);

    // boolean action
    PropertyType[ord(TProp.Reset)] := TPropertyType.BooleanActionProperty;
    PropertyOffset[ord(TProp.Reset)] := ptruint(@DoReset);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TCapControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TCapControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    // PF Controller changes
    if ControlType = PFCONTROL then // TODO: check -- is this correct for all below??
        with ControlVars do
            case Idx of
                ord(TProp.typ):
                begin
                    PFON_Value := 0.95;     // defaults
                    PFOFF_Value := 1.05;
                end;
                ord(TProp.ONsetting):
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
                        DoSimpleMsg('Invalid PF ON value for "%s"', [FullName], 353);
                    end;
                end;
                ord(TProp.OFFsetting):
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
                        DoSimpleMsg('Invalid PF OFF value for "%s"', [FullName], 35301);
                    end;
                end;
                ord(TProp.CTPhase):
                    if FCTPhase > FNphases then
                    begin
                        DoSimpleMsg('Error: Monitored phase (%d) must be less than or equal to number of phases (%d). ', [FCTPhase, FNphases], 35302);
                        FCTPhase := 1;
                    end;
                ord(TProp.PTPhase):
                    if FPTPhase > FNphases then
                    begin
                        DoSimpleMsg('Error: Monitored phase (%d) must be less than or equal to number of phases (%d). ', [FPTPhase, FNphases], 35303);
                        FPTPhase := 1;
                    end;
            end;

    case Idx of
        ord(TProp.Capacitor):
            if ControlledElement <> NIL then
                ControlVars.CapacitorName := 'capacitor.' + ControlledElement.Name;
        ord(TProp.VBus):
        begin
            ControlVars.VOverrideBusName := AnsiLowerCase(ControlVars.VOverrideBusName);
            ControlVars.VoverrideBusSpecified := TRUE;
        end;
        ord(TProp.UserModel):
        begin
            UserModel.Name := UserModelNameStr;  // Connect to user written model
            IsUserModel := UserModel.Exists;
        end;
        ord(TProp.UserData):
            if UserModel.Exists then
                UserModel.Edit := UserModelEditStr;  // Send edit string to user model
    end;

    if IsUserModel then
        ControlType := USERCONTROL;

    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TCapControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    ControlVars.CapacitorName := Other.ControlVars.CapacitorName;
    ControlledElement := Other.ControlledElement;  // Pointer to target circuit element
    MonitoredElement := Other.MonitoredElement;  // Pointer to target circuit element

    ElementTerminal := Other.ElementTerminal;
    with ControlVars do
    begin
        PTRatio := Other.ControlVars.PTRatio;
        CTRatio := Other.ControlVars.CTRatio;
        ControlType := Other.ControlType;
        PresentState := Other.ControlVars.PresentState;
        ShouldSwitch := Other.ControlVars.ShouldSwitch;
        CondOffset := Other.ControlVars.CondOffset;

        ON_Value := Other.ControlVars.ON_Value;
        OFF_Value := Other.ControlVars.OFF_Value;
        PFON_Value := Other.ControlVars.PFON_Value;
        PFOFF_Value := Other.ControlVars.PFOFF_Value;

        FCTPhase := Other.ControlVars.FCTPhase;
        FPTPhase := Other.ControlVars.FPTPhase;

        Voverride := Other.ControlVars.Voverride;
        VoverrideBusSpecified := Other.ControlVars.VoverrideBusSpecified;     // Added 8-11-11
        VOverrideBusName := Other.ControlVars.VOverrideBusName;
    end;

    UserModel.Name := Other.UserModel.Name;  // Connect to user written models
    UserModelNameStr := Other.UserModelNameStr;

    IsUserModel := Other.IsUserModel;

    FpctMinkvar := Other.FpctMinkvar;

    ShowEventLog := Other.ShowEventLog;
end;

constructor TCapControlObj.Create(ParClass: TDSSClass; const CapControlName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(CapControlName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
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

    ControlledElement := NIL;
    ElementTerminal := 1;
    ControlVars.CapacitorName := '';
    MonitoredElement := NIL;

    FpctMinkvar := 50.0;

    IsUserModel := FALSE;
    UserModel := TCapUserControl.Create(DSS);   // Inits handles, FID
    UserModelNameStr := '';

    ControlVars.ControlActionHandle := 0;

    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

   //  RecalcElementData;
end;

destructor TCapControlObj.Destroy;
begin
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

procedure TCapControlObj.RecalcElementData;
begin
    // Check for existence of capacitor

    // 5-21-01 RCD moved this section ahead of monitored element so Nphases gets defined first

    if ControlledElement = NIL then
        raise Exception.Create(Format(_('CapControl "%s": Capacitor is not set, aborting.'), [Name]));

    if MonitoredElement = NIL then
        raise Exception.Create(Format(_('CapControl "%s": Element is not set, aborting.'), [Name]));

    // Both capacitor and monitored element must already exist
    ControlledCapacitor := This_Capacitor;
    FNphases := ControlledElement.NPhases;  // Force number of phases to be same   Added 5/21/01  RCD
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

    ControlVars.InitialState := ControlVars.PresentState;


    if ElementTerminal > MonitoredElement.Nterms then
    begin
        DoErrorMsg(FullName,
            Format(_('Terminal no. "%s" does not exist.'), [ElementTerminal]),
            _('Re-specify terminal no.'), 362);
    end
    else
    begin
        // Sets name of i-th terminal's connected bus in CapControl's buslist
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        // Allocate a buffer bigenough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
        ControlVars.CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
    end;

    // Alternative override bus
    if ControlVars.VoverrideBusSpecified then
        with ControlVars do
        begin
            VOverrideBusIndex := ActiveCircuit.BusList.Find(VOverrideBusName);
            if VOverrideBusIndex = 0 then
            begin
                DoSimpleMsg('%s: Voltage override Bus "%s" not found. Did you wait until buses were defined? Reverting to default.', [FullName, VOverrideBusName], 10361);
                VoverrideBusSpecified := FALSE;
            end;

        end;

    // User model property update, if necessary
    if Usermodel.Exists then
        UserModel.UpdateModel;  // Checks for existence and Selects

end;

procedure TCapControlObj.MakePosSequence();
begin
    if ControlledElement <> NIL then
    begin
        Enabled := ControlledElement.Enabled;
        FNphases := ControlledElement.NPhases;
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

procedure TCapControlObj.GetBusVoltages(pBus: TDSSBus; Buff: pComplexArray);
var
    j: Integer;
begin
    with pBus do
        if Assigned(Vbus) then    // uses nphases from CapControlObj
            for j := 1 to nPhases do //TODO (@meira) check if nphases is ambiguous
                cBuffer^[j] := ActiveCircuit.Solution.NodeV^[GetRef(j)];
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
            // Just use one phase because that's what most controls do.
            ControlCurrent := Cabs(Cbuffer^[FCTphase]) / CTRatio;  // monitored phase only
        end;
end;

procedure TCapControlObj.DoPendingAction(const Code, ProxyHdl: Integer);
begin
    ControlledElement.ActiveTerminalIdx := 1;  // Set active terminal of capacitor to terminal 1

    // Allow user control to do something
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
                                AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Opened**');
                            PresentState := CTRL_OPEN;
                            with ActiveCircuit.Solution do
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
                                AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Opened**');
                        end
                        else
                        if ShowEventLog then
                            AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Step Down**');
                    end;
                end;
            CTRL_CLOSE:
            begin
                if PresentState = CTRL_OPEN then
                begin
                    ControlledElement.Closed[0] := TRUE;    // Close all phases of active terminal
                    if ShowEventLog then
                        AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Closed**');
                    PresentState := CTRL_CLOSE;
                    ControlledCapacitor.AddStep;
                end
                else
                begin
                    if ControlledCapacitor.AddStep then
                        if ShowEventLog then
                            AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Step Up**');
                end;
            end;
        else
            // Do Nothing for NONE if the control has reset
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
            // Just use one phase because that's what most controls do.
            // Use L-L aB if capacitor is delta connected!!
            case TCapacitorObj(ControlledElement).Connection of
                TCapacitorConnection.Delta:
                    ControlVoltage := Cabs(cBuffer^[FPTPhase] - cBuffer^[NextDeltaPhase(FPTPhase)]) / PTRatio;
            else
                ControlVoltage := Cabs(cBuffer^[FPTPhase]) / PTRatio;     // Wye - Default
            end;
        end;
end;

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

    with MonitoredElement, ControlVars do
    begin
        ShouldSwitch := FALSE;

        // First Check voltage override
        if Voverride then
            if ControlType <> VOLTAGECONTROL then
            begin  // Don't bother for voltage control

                if VoverrideBusSpecified then
                begin
                    GetBusVoltages(ActiveCircuit.Buses^[VOverrideBusIndex], cBuffer);
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
                                AppendtoEventLog('Capacitor.' + ControlledElement.Name, Format('Low Voltage Override: %.8g V', [Vtest]));
                        end;
                    CTRL_CLOSE:
                        if Vtest > Vmax then
                        begin
                            PendingChange := CTRL_OPEN;
                            ShouldSwitch := TRUE;
                            VoverrideEvent := TRUE;
                            if ShowEventLog then
                                AppendtoEventLog('Capacitor.' + ControlledElement.Name, Format('High Voltage Override: %.8g V', [Vtest]));
                        end;
                end;
            end;

        if not ShouldSwitch then   // Else skip other control evaluations
            case ControlType of
                CURRENTCONTROL:
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

                VOLTAGECONTROL:
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

                KVARCONTROL:
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
                USERCONTROL:
                    if UserModel.Exists then   // selects the model associated with this control
                    begin
                        // Load up test data into the public data record
                        SampleP := MonitoredElement.Power[ElementTerminal] * 0.001;  // kW kvar

                        MonitoredElement.GetTermVoltages(ElementTerminal, cBuffer);
                        GetControlVoltage(SampleV);

                        MonitoredElement.GetCurrents(cBuffer);
                        GetControlCurrent(SampleCurr);

                        NumCapSteps := ControlledCapacitor.NumSteps;
                        AvailableSteps := ControlledCapacitor.AvailableSteps;
                        LastStepInService := ControlledCapacitor.LastStepInService;

                        UserModel.Sample;   // Sets the switching flags
                    end;
                TIMECONTROL:
                begin
                    with ActiveCircuit.Solution do
                    begin
                        NormalizedTime := NormalizeToTOD(DynaVars.intHour, DynaVars.t);
                    end;
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

                    // PF is in range of 0 .. 2;  Leading is 1..2
                    // When turning on make sure there is at least half the kvar of the bank

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
    with ActiveCircuit, ControlVars do
    begin
        if ShouldSwitch and not Armed then
        begin
            if PendingChange = CTRL_CLOSE then
            begin
                if (Solution.DynaVars.t + Solution.DynaVars.intHour * 3600.0 - LastOpenTime) < DeadTime then // delay the close operation
                    TimeDelay := Max(ONDelay, (Deadtime + ONDelay) - (Solution.DynaVars.t + Solution.DynaVars.intHour * 3600.0 - LastOpenTime))
                else
                    TimeDelay := ONDelay;
            end
            else
                TimeDelay := OFFDelay;
            ControlActionHandle := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TimeDelay, PendingChange, 0, Self);
            Armed := TRUE;
            if ShowEventLog then
                AppendtoEventLog('Capacitor.' + ControlledElement.Name, Format('**Armed**, Delay= %.5g sec', [TimeDelay]));
        end;

        if Armed and (PendingChange = CTRL_NONE) then
        begin
            ControlQueue.Delete(ControlActionHandle);
            Armed := FALSE;
            if ShowEventLog then
                AppendtoEventLog('Capacitor.' + ControlledElement.Name, '**Reset**');
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

procedure TCapControlObj.Set_PendingChange(const Value: EControlAction);
begin
    ControlVars.FPendingChange := Value;
    DblTraceParameter := Integer(Value);
end;

procedure TCapControlObj.Set_Enabled(Value: Boolean);
begin
    // Do nothing else besides toggling the flag,
    // we don't need BusNameRedefined from CktElement.pas
    FEnabled := Value;
end;

finalization    TypeEnum.Free;
end.
