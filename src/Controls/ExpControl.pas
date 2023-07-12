unit ExpControl;

// ----------------------------------------------------------
// Copyright (c) 2015-2016, University of Pittsburgh
// Copyright (c) 2019-2020, Battelle Memorial Institute
// All rights reserved.
// ----------------------------------------------------------

// Notes: adapted and simplified from InvControl for adaptive controller research

interface

uses
    gqueue,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    PVSystem,
    Arraydef,
    UComplex,
    DSSUcomplex,
    utilities,
    Dynamics,
    DSSPointerList,
    Classes,
    StrUtils;

type
{$SCOPEDENUMS ON}
    TExpControlPropLegacy = (
        INVALID = 0,
        PVSystemList = 1,
        Vreg = 2,
        Slope = 3,
        VregTau = 4,
        Qbias = 5,
        VregMin = 6,
        VregMax = 7,
        QmaxLead = 8,
        QmaxLag = 9,
        EventLog = 10,
        DeltaQ_factor = 11,
        PreferQ = 12,
        Tresponse = 13,
        DERList = 14
        );
    TExpControlProp = (
        INVALID = 0,
        PVSystemList = 1,
        VReg = 2,
        Slope = 3,
        VRegTau = 4,
        QBias = 5,
        VRegMin = 6,
        VRegMax = 7,
        QMaxLead = 8,
        QMaxLag = 9,
        EventLog = 10,
        DeltaQ_Factor = 11,
        PreferQ = 12,
        TResponse = 13,
        DERList = 14
        );
{$SCOPEDENUMS OFF}

    TExpControl = class(TControlClass)
    PROTECTED
        procedure DefineProperties; OVERRIDE;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function NewObject(const ObjName: String; Activate: Boolean = TRUE): Pointer; OVERRIDE;
        procedure UpdateAll;
    end;

    TExpControlObj = class(TControlElem)
    PRIVATE
        ControlActionHandle: Integer;
        ControlledElement: array of TPVSystemObj;    // list of pointers to controlled PVSystem elements
        MonitoredElement: TDSSCktElement;  // First PVSystem element for now

            // PVSystemList information
        FListSize: Integer;
        FPVSystemPointerList: TDSSPointerList;

            // working storage for each PV system under management
        FPriorVpu: array of Double;
        FPresentVpu: array of Double;
        FPendingChange: array of Integer;
        FVregs: array of Double;
        FLastIterQ: array of Double; // for DeltaQFactor
        FLastStepQ: array of Double; // for FOpenTau
        FTargetQ: array of Double;
        FWithinTol: array of Boolean;

            // temp storage for biggest PV system, not each one
        cBuffer: array of Complex;

            // user-supplied parameters (also PVSystemList and EventLog)
        FVregInit: Double;
        FQbias: Double;
        FdeltaQ_factor: Double;
        FVoltageChangeTolerance: Double; // no user adjustment
        FVarChangeTolerance: Double;     // no user adjustment
        FPreferQ: Longbool;

        FOpenTau: Double;

        procedure Set_PendingChange(Value: Integer; DevIndex: Integer);
        function Get_PendingChange(DevIndex: Integer): Integer;
        procedure UpdateExpControl(i: Integer);
    PUBLIC
        FPVSystemNameList, DERNameList: TStringList;
        QmaxLead: Double;
        QmaxLag: Double;
        VregMin: Double;
        VregMax: Double;
        VregTau: Double;
        QVSlope: Double; // originally FSlope
        Tresponse: Double;

        constructor Create(ParClass: TDSSClass; const ExpControlName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); OVERRIDE;
        procedure MakeLike(OtherPtr: Pointer); OVERRIDE;

            // PROCEDURE Set_Enabled(Value: Boolean);Override;
        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

            // Sample control quantities and set action times in Control Queue
        procedure Sample; OVERRIDE;

            // Do the action that is pending from last sample
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;

        procedure Reset; OVERRIDE;  // Reset to initial defined state

        function MakePVSystemList(doRecalc: Boolean = TRUE): Boolean;

        property PendingChange[DevIndex: Integer]: Integer READ Get_PendingChange WRITE Set_PendingChange;

    end;

implementation

uses
    Sysutils,
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    uCmatrix,
    MathUtil,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TExpControlObj;
    TProp = TExpControlProp;
    TPropLegacy = TExpControlPropLegacy;

const
    NumPropsThisClass = Ord(High(TProp));
    NONE = 0;
    CHANGEVARLEVEL = 1;

var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;

constructor TExpControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, EXP_CONTROL, 'ExpControl');
end;

destructor TExpControl.Destroy;
begin
    inherited Destroy;
end;

procedure TExpControl.DefineProperties;
var
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    // string lists
    PropertyType[ord(TProp.PVSystemList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.PVSystemList)] := ptruint(@obj.FPVSystemNameList);

    PropertyType[ord(TProp.DERList)] := TPropertyType.StringListProperty;
    PropertyOffset[ord(TProp.DERList)] := ptruint(@obj.DERNameList);

    // boolean properties
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.PreferQ)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);
    PropertyOffset[ord(TProp.PreferQ)] := ptruint(@obj.FPreferQ);

    // double properties (default type)
    PropertyOffset[ord(TProp.Qbias)] := ptruint(@obj.FQbias);
    PropertyOffset[ord(TProp.DeltaQ_factor)] := ptruint(@obj.FdeltaQ_factor);

    // advanced doubles
    PropertyOffset[ord(TProp.Vreg)] := ptruint(@obj.FVregInit);
    PropertyFlags[ord(TProp.Vreg)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.Slope)] := ptruint(@obj.QVSlope);
    PropertyFlags[ord(TProp.Slope)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyOffset[ord(TProp.VregTau)] := ptruint(@obj.VregTau);
    PropertyFlags[ord(TProp.VregTau)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.Units_s];

    PropertyOffset[ord(TProp.VregMin)] := ptruint(@obj.VregMin);
    PropertyFlags[ord(TProp.VregMin)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.VregMax)] := ptruint(@obj.VregMax);
    PropertyFlags[ord(TProp.VregMax)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.QmaxLead)] := ptruint(@obj.QmaxLead);
    PropertyFlags[ord(TProp.QmaxLead)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyOffset[ord(TProp.QmaxLag)] := ptruint(@obj.QmaxLag);
    PropertyFlags[ord(TProp.QmaxLag)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    PropertyOffset[ord(TProp.Tresponse)] := ptruint(@obj.Tresponse);
    PropertyFlags[ord(TProp.Tresponse)] := [TPropertyFlag.IgnoreInvalid, TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TExpControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TExpControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
        ord(TProp.PVSystemList):
        begin
            FPVSystemPointerList.Clear; // clear this for resetting on first sample
            FListSize := FPVSystemNameList.count;
            DERNameList.Clear;
            for i := 0 to FListSize - 1 do
                DerNameList.Add('PVSystem.' + FPVSystemNameList[i]);
        end;
        ord(TProp.DERList):
        begin
            FPVSystemPointerList.Clear; // clear this for resetting on first sample
            FListSize := DERNameList.count;
            FPVSystemNameList.Clear;
            for i := 0 to FListSize - 1 do
                FPVSystemNameList.Add(StripClassName(DERNameList[i]));
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TExpControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff
    for i := 1 to FPVSystemPointerList.Count do
    begin
        ControlledElement[i] := Other.ControlledElement[i];
        FWithinTol[i] := Other.FWithinTol[i];
    end;
    FListSize := Other.FListSize;
    FVoltageChangeTolerance := Other.FVoltageChangeTolerance;
    FVarChangeTolerance := Other.FVarChangeTolerance;
    FVregInit := Other.FVregInit;
    QVSlope := Other.QVSlope;
    VregTau := Other.VregTau;
    FQbias := Other.FQbias;
    VregMin := Other.VregMin;
    VregMax := Other.VregMax;
    QmaxLead := Other.QmaxLead;
    QmaxLag := Other.QmaxLag;
    FdeltaQ_factor := Other.FdeltaQ_factor;
    FPreferQ := Other.FPreferQ;
end;

constructor TExpControlObj.Create(ParClass: TDSSClass; const ExpControlName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(ExpControlName);
    DSSObjType := ParClass.DSSClassType;

     //  Control elements are zero current sources that attach to a terminal of a
     //  power-carrying device, but do not alter voltage or current flow.
     //  Define a default number of phases and conductors here and update in
     //  RecalcElementData routine if necessary. This allocates arrays for voltages
     //  and currents and gives more direct access to the values, if needed
    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors
     // This general feature should not be used for ExpControl,
     // because it controls more than one PVSystem

    ShowEventLog := FALSE;

    ControlledElement := NIL;
    FPVSystemNameList := NIL;
    FPVSystemPointerList := NIL;
    cBuffer := NIL;
    FPriorVpu := NIL;
    FPresentVpu := NIL;
    FPendingChange := NIL;
    FLastIterQ := NIL;
    FLastStepQ := NIL;
    FTargetQ := NIL;
    FWithinTol := NIL;

    FVoltageChangeTolerance := 0.0001;  // per-unit
    FVarChangeTolerance := 0.0001;  // per-unit

    FPVSystemNameList := TStringList.Create;
    DERNameList := TStringList.Create;
    FPVSystemPointerList := TDSSPointerList.Create(20);  // Default size and increment


     // user parameters for dynamic Vreg
    FVregInit := 1.0; // 0 means to find it during initialization
    QVSlope := 50.0;
    VregTau := 1200.0;
    FVregs := NIL;
    FQbias := 0.0;
    VregMin := 0.95;
    VregMax := 1.05;
    QmaxLead := 0.44;
    QmaxLag := 0.44;
    FdeltaQ_factor := 0.7; // only on control iterations, not the final solution
    FPreferQ := FALSE;
    Tresponse := 0.0;
    FOpenTau := 0.0;

     //generic for control
    FPendingChange := NIL;
end;

destructor TExpControlObj.Destroy;
begin
    FreeAndNil(FPVSystemPointerList);
    FreeAndNil(FPVSystemNameList);
    FreeAndNil(DERNameList);
    Finalize(ControlledElement);
    Finalize(cBuffer);
    Finalize(FPriorVpu);
    Finalize(FPresentVpu);
    Finalize(FPendingChange);
    Finalize(FLastIterQ);
    Finalize(FLastStepQ);
    Finalize(FTargetQ);
    Finalize(FWithinTol);
    Finalize(FVregs);
    inherited Destroy;
end;

procedure TExpControlObj.RecalcElementData;
var
    i: Integer;
    maxord: Integer;
begin
    FOpenTau := Tresponse / 2.3026;
    if FPVSystemPointerList.Count = 0 then
        MakePVSystemList(FALSE);

    if FPVSystemPointerList.Count > 0 then
    begin
    // Setting the terminal of the ExpControl device to same as the 1st PVSystem element
        MonitoredElement := TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
        Setbus(1, MonitoredElement.Firstbus);
    end;

    maxord := 0; // will be the size of cBuffer
    for i := 1 to FPVSystemPointerList.Count do
    begin
        // User ControlledElement[] as the pointer to the PVSystem elements
        ControlledElement[i] := TPVSystemObj(FPVSystemPointerList.Get(i));  // pointer to i-th PVSystem
        FNphases := ControlledElement[i].NPhases;  // TEMC TODO - what if these are different sizes (same concern exists with InvControl)
        Nconds := Nphases;
        if (ControlledElement[i] = NIL) then
            DoErrorMsg(Format(_('ExpControl: "%s"'), [Self.Name]),
                Format(_('Controlled Element "%s" not found.'), [FPVSystemNameList.Strings[i - 1]]),
                _('PVSystem object must be defined previously.'), 361);
        if ControlledElement[i].Yorder > maxord then
            maxord := ControlledElement[i].Yorder;
        ControlledElement[i].ActiveTerminalIdx := 1; // Make the 1 st terminal active
    end;
    if maxord > 0 then
        SetLength(cBuffer, SizeOF(Complex) * maxord);
end;

procedure TExpControlObj.MakePosSequence();
// ***  This assumes the PVSystem devices have already been converted to pos seq
begin
    if FPVSystemPointerList.Count = 0 then
        RecalcElementData;
  // TEMC - from here to inherited was copied from InvControl
    FNphases := 3;
    Nconds := 3;
    Setbus(1, MonitoredElement.GetBus(ElementTerminal));
    if FPVSystemPointerList.Count > 0 then
    begin
    // Setting the terminal of the ExpControl device to same as the 1st PVSystem element
    // This sets it to a realistic value to avoid crashes later
        MonitoredElement := TDSSCktElement(FPVSystemPointerList.Get(1));   // Set MonitoredElement to 1st PVSystem in lise
        Setbus(1, MonitoredElement.Firstbus);
        FNphases := MonitoredElement.NPhases;
        Nconds := Nphases;
    end;
    inherited;
end;

procedure TExpControlObj.DoPendingAction;
var
    i: Integer;
    Qset, DeltaQ: Double;
    Qmaxpu, Qpu: Double;
    Qbase: Double;
    Qinvmaxpu: Double;
    Plimit: Double;
    dt: Double;
    PVSys: TPVSystemObj;
begin
    for i := 1 to FPVSystemPointerList.Count do
    begin
        PVSys := ControlledElement[i];   // Use local variable in loop
        if PendingChange[i] = CHANGEVARLEVEL then
        begin
            PVSys.VWmode := FALSE;
            PVSys.ActiveTerminalIdx := 1; // Set active terminal of PVSystem to terminal 1
            PVSys.Varmode := VARMODEKVAR;  // Set var mode to VARMODEKVAR to indicate we might change kvar
            FTargetQ[i] := 0.0;
            Qbase := PVSys.kVARating;
            Qinvmaxpu := PVSys.kvarLimit / Qbase;
            Qpu := PVSys.Presentkvar / Qbase; // no change for now

            if (FWithinTol[i] = FALSE) then
            begin
        // look up Qpu from the slope crossing at Vreg, and add the bias
                Qpu := -QVSlope * (FPresentVpu[i] - FVregs[i]) + FQbias;
                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ',' + PVSys.Name,
                        Format(' Setting Qpu= %.5g at FVreg= %.5g, Vpu= %.5g', [Qpu, FVregs[i], FPresentVpu[i]]));
            end;

      // apply limits on Qpu, then define the target in kVAR
            PVSys.SetNominalDEROutput; // as does InvControl
            if FPreferQ then
                Qmaxpu := 1.0
            else
                Qmaxpu := Sqrt(1 - Sqr(PVSys.PresentkW / Qbase)); // dynamic headroom
            if Qmaxpu > Qinvmaxpu then
                Qmaxpu := Qinvmaxpu;
            if Abs(Qpu) > Qmaxpu then
                Qpu := QmaxPu * Sign(Qpu);
            if Qpu < -QmaxLead then
                Qpu := -QmaxLead;
            if Qpu > QmaxLag then
                Qpu := QmaxLag;
            FTargetQ[i] := Qbase * Qpu;
            if FPreferQ then
            begin
                Plimit := Qbase * Sqrt(1 - Qpu * Qpu);
                if Plimit < PVSys.PresentkW then
                begin
                    if ShowEventLog then
                        AppendtoEventLog(Self.FullName + ',' + PVSys.Name,
                            Format(' curtailing %.3f to %.3f kW', [PVSys.PresentkW, Plimit]));
                    PVSys.PresentkW := Plimit;
                    PVSys.puPmpp := Plimit / PVSys.Pmpp;
                end;
            end;

      // put FTargetQ through the low-pass open-loop filter
            if FOpenTau > 0.0 then
                if (ActiveCircuit.Solution.ControlMode <> CTRLSTATIC) then
                begin
                    dt := ActiveCircuit.Solution.Dynavars.h;
                    FTargetQ[i] := FLastStepQ[i] + (FTargetQ[i] - FLastStepQ[i]) * (1 - Exp(-dt / FOpenTau)); // TODO - precalculate?
                end;

      // only move the non-bias component by deltaQ_factor in this control iteration
            DeltaQ := FTargetQ[i] - FLastIterQ[i];
            Qset := FLastIterQ[i] + DeltaQ * FdeltaQ_factor;
 //     Qset := FQbias * Qbase;
            if PVSys.Presentkvar <> Qset then
                PVSys.Presentkvar := Qset;
            if ShowEventLog then
                AppendtoEventLog(Self.FullName + ',' + PVSys.Name,
                    Format(' Setting PVSystem output kvar= %.5g',
                    [PVSys.Presentkvar]));
            FLastIterQ[i] := Qset;
            FPriorVpu[i] := FPresentVpu[i];
            ActiveCircuit.Solution.LoadsNeedUpdating := TRUE;
      // Force recalc of power parms
            Set_PendingChange(NONE, i);
        end
    end;
end;

procedure TExpControlObj.Sample;
var
    i, j: Integer;
    basekV, Vpresent: Double;
    Verr, Qerr: Double;
    PVSys: TPVSystemObj;
begin
  // If list is not defined, go make one from all PVSystem in circuit
    if FPVSystemPointerList.Count = 0 then
        RecalcElementData;

    if (FListSize > 0) then
    begin
    // If an ExpControl controls more than one PV, control each one
    // separately based on the PVSystem's terminal voltages, etc.
        for i := 1 to FPVSystemPointerList.Count do
        begin
            PVSys := ControlledElement[i];   // Use local variable in loop
      // Calculate the present average voltage  magnitude
            PVSys.ComputeVTerminal;
            for j := 1 to PVSys.Yorder do
                cBuffer[j] := PVSys.Vterminal[j];
            BasekV := ActiveCircuit.Buses[PVSys.terminals[0].busRef].kVBase;
            Vpresent := 0;
            for j := 1 to PVSys.NPhases do
                Vpresent := Vpresent + Cabs(cBuffer[j]);
            FPresentVpu[i] := (Vpresent / PVSys.NPhases) / (basekV * 1000.0);
      // if initializing with Vreg=0 in static mode, we want to FIND Vreg
            if (ActiveCircuit.Solution.ControlMode = CTRLSTATIC) and (FVregInit <= 0.0) then
            begin
                FVregs[i] := FPresentVpu[i];
                if FVregs[i] < VregMin then
                begin
                    FVregs[i] := VregMin;
                    FVregInit := 0.01; // don't let it outside the band
                end;
                if FVregs[i] > VregMax then
                begin
                    FVregs[i] := VregMax;
                    FVregInit := 0.01; // don't let it outside the band
                end;
            end;
      // both errors are in per-unit
            Verr := Abs(FPresentVpu[i] - FPriorVpu[i]);
            Qerr := Abs(PVSys.Presentkvar - FTargetQ[i]) / PVSys.kVARating;
      // process the sample
            if (PVSys.InverterON = FALSE) and (PVSys.VarFollowInverter = TRUE) then
            begin // not injecting
                if (VregTau > 0.0) and (FVregs[i] <= 0.0) then
                    FVregs[i] := FPresentVpu[i]; // wake up to the grid voltage, otherwise track it while not injecting
                continue;
            end;
            PVSys.VWmode := FALSE;
            if ((Verr > FVoltageChangeTolerance) or (Qerr > FVarChangeTolerance) or
                (ActiveCircuit.Solution.ControlIteration = 1)) then
            begin
                FWithinTol[i] := FALSE;
                Set_PendingChange(CHANGEVARLEVEL, i);
                ControlActionHandle := ActiveCircuit.ControlQueue.Push(TimeDelay, PendingChange[i], 0, Self);
                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ' ' + PVSys.Name, Format(' outside Hit Tolerance, Verr= %.5g, Qerr=%.5g', [Verr, Qerr]));
            end
            else
            begin
                FWithinTol[i] := TRUE;
                if ShowEventLog then
                    AppendtoEventLog(Self.FullName + ' ' + PVSys.Name, Format(' within Hit Tolerance, Verr= %.5g, Qerr=%.5g', [Verr, Qerr]));
            end;
        end;  // For
    end; // If FlistSize
end;

function TExpControlObj.MakePVSystemList(doRecalc: Boolean): Boolean;
var
    PVSysClass: TDSSClass;
    PVSys: TPVsystemObj;
    i: Integer;
begin
    Result := FALSE;
    PVSysClass := GetDSSClassPtr(DSS, 'PVsystem');
    if FListSize > 0 then
    begin    // Name list is defined - Use it
        SetLength(ControlledElement, FListSize + 1);  // Use this as the main pointer to PVSystem Elements
        SetLength(FPriorVpu, FListSize + 1);
        SetLength(FPresentVpu, FListSize + 1);
        SetLength(FPendingChange, FListSize + 1);
        SetLength(FLastIterQ, FListSize + 1);
        SetLength(FLastStepQ, FListSize + 1);
        SetLength(FTargetQ, FListSize + 1);
        SetLength(FWithinTol, FListSize + 1);
        SetLength(FVregs, FListSize + 1);
        for i := 1 to FListSize do
        begin
            PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i - 1]);
            if Assigned(PVSys) and PVSys.Enabled then
            begin
                FPVSystemPointerList.Add(PVSys);
                PVSys.AVRmode := TRUE;
            end;
        end;
    end
    else
    begin
         // Search through the entire circuit for enabled pvsysten objects and add them to the list
        for i := 1 to PVSysClass.ElementCount do
        begin
            PVSys := PVSysClass.ElementList.Get(i);
            if PVSys.Enabled then
            begin
                FPVSystemPointerList.Add(PVSys);
                PVSys.AVRmode := TRUE;
            end;
            FPVSystemNameList.Add(PVSys.Name);
        end;
        FListSize := FPVSystemPointerList.Count;

        SetLength(ControlledElement, FListSize + 1);

        SetLength(FPriorVpu, FListSize + 1);
        SetLength(FPresentVpu, FListSize + 1);

        SetLength(FPendingChange, FListSize + 1);
        SetLength(FLastIterQ, FListSize + 1);
        SetLength(FLastStepQ, FListSize + 1);
        SetLength(FTargetQ, FListSize + 1);
        SetLength(FWithinTol, FListSize + 1);
        SetLength(FVregs, FListSize + 1);
    end;  // Else

  //Initialize arrays
    for i := 1 to FlistSize do
    begin
    // PVSys := PVSysClass.Find(FPVSystemNameList.Strings[i-1]);
    // Set_NTerms(PVSys.NTerms); // TODO - what is this for?
        FPriorVpu[i] := 0.0;
        FPresentVpu[i] := 0.0;
        FLastIterQ[i] := -1.0;
        FLastStepQ[i] := -1.0;
        FTargetQ[i] := 0.0;
        FWithinTol[i] := FALSE;
        FVregs[i] := FVregInit;
        FPendingChange[i] := NONE;
    end;
    if doRecalc then
        RecalcElementData;

    if FPVSystemPointerList.Count > 0 then
        Result := TRUE;
end;

procedure TExpControlObj.Reset;
begin
  // inherited;
end;

//procedure TExpControlObj.Set_Enabled(Value: Boolean);
//begin
//    inherited;
//    // Reset controlled PVSystems to original PF
//end;

procedure TExpControlObj.Set_PendingChange(Value: Integer; DevIndex: Integer);
begin
    FPendingChange[DevIndex] := Value;
    DblTraceParameter := Value;
end;

procedure TExpControlObj.UpdateExpControl(i: Integer);
var
    j: Integer;
    PVSys: TPVSystemObj;
    dt, Verr: Double; // for DYNAMICVREG
begin
    for j := 1 to FPVSystemPointerList.Count do
    begin
        PVSys := ControlledElement[j];
        FLastStepQ[j] := PVSys.Presentkvar;
        if VregTau > 0.0 then
        begin
            dt := ActiveCircuit.Solution.Dynavars.h;
            Verr := FPresentVpu[j] - FVregs[j];
            FVregs[j] := FVregs[j] + Verr * (1 - Exp(-dt / VregTau)); // TODO - precalculate?
        end
        else
        begin
            Verr := 0.0;
        end;
        if FVregs[j] < VregMin then
            FVregs[j] := VregMin;
        if FVregs[j] > VregMax then
            FVregs[j] := VregMax;
        PVSys.Set_Variable(5, FVregs[j]);
        if ShowEventLog then
            AppendtoEventLog(Self.FullName + ',' + PVSys.Name,
                Format(' Setting new Vreg= %.5g Vpu=%.5g Verr=%.5g',
                [FVregs[j], FPresentVpu[j], Verr]));
    end;
end;

function TExpControlObj.Get_PendingChange(DevIndex: Integer): Integer;
begin
    Result := FPendingChange[DevIndex];
end;

//Called at end of main power flow solution loop
procedure TExpControl.UpdateAll;
var
    i: Integer;
    obj: TExpControlObj;
begin
    for i := 1 to ElementList.Count do
    begin
        obj := ElementList.Get(i);
        if obj.Enabled then
            obj.UpdateExpControl(i);
    end;
end;

end.
