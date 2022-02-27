unit RegControl;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

//   12/4/2018  Added autotransformer control

//  A RegControl is a control element that is connected to a terminal of another
//  circuit element that must be a transformer.
//
//  A RegControl is defined by a New command:
//
//  New RegControl.Name=myname Transformer = name Terminal=[1,2,...] Controlledbus=name etc...
//
//  Transformer to be controlled must already exist.

interface

uses
    Classes,
    Command,
    ControlClass,
    ControlElem,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    Transformer,
    AutoTrans,
    utilities;

type
{$SCOPEDENUMS ON}
    TRegControlProp = (
        INVALID = 0,
        transformer = 1,
        winding = 2,
        vreg = 3,
        band = 4,
        ptratio = 5,
        CTprim = 6,
        R = 7,
        X = 8,
        bus = 9,
        delay = 10,
        reversible = 11,
        revvreg = 12,
        revband = 13,
        revR = 14,
        revX = 15,
        tapdelay = 16,
        debugtrace = 17,
        maxtapchange = 18,
        inversetime = 19,
        tapwinding = 20,
        vlimit = 21,
        PTphase = 22,
        revThreshold = 23,
        revDelay = 24,
        revNeutral = 25,
        EventLog = 26,
        RemotePTRatio = 27,
        TapNum = 28,
        Reset = 29,
        LDC_Z = 30,
        rev_Z = 31,
        Cogen = 32 
    );
{$SCOPEDENUMS OFF}

    TRegControl = class(TControlClass)
    PROTECTED
        Transf_Or_AutoTrans_ProxyClass: TProxyClass;

        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TRegControlObj = class(TControlElem)
    PUBLIC
        LastChange: Integer;

        RemotePTRatio,
        LDC_Z: Double;

        // Reverse Power Variables
        RevPowerThreshold,   // W
        kWRevPowerThreshold,
        revDelay,
        revLDC_Z: Double;

        InReverseMode: Boolean;
        ReversePending: Boolean;
        ReverseNeutral: Boolean;
        CogenEnabled: Boolean;
        InCogenMode: Boolean;

        RevHandle: Integer;
        RevBackHandle: Integer;

        UsingRegulatedBus: Boolean;
        RegulatedBus: String;

        FPendingTapChange: Double;   // amount of tap change pending

        DebugTrace: Boolean;
        Armed: Boolean;
        TraceFile: TFileStream;

        TapWinding: Integer;

        FPTphase: Integer;
        ControlledPhase: Integer;

        ControlActionHandle: Integer;

        VBuffer, CBuffer: pComplexArray;

        procedure Set_Enabled(Value: Boolean); OVERRIDE;

        function Get_Transformer: TTransfObj;
        function Get_Winding: Integer;
        function Get_MinTap: Double;
        function Get_MaxTap: Double;
        function Get_TapIncrement: Double;
        function Get_NumTaps: Integer;
        function Get_TapNum: Integer;
        procedure Set_TapNum(const Value: Integer);

        procedure RegWriteTraceRecord(TapChangeMade: Double);
        procedure RegWriteDebugRecord(S: String);
        procedure set_PendingTapChange(const Value: Double);
        function AtLeastOneTap(const ProposedChange: Double; Increment: Double): Double;
        function ComputeTimeDelay(Vavg: Double): Double;
        function GetControlVoltage(VBuffer: pComplexArray; Nphs: Integer; PTRatio: Double): Complex;
    PUBLIC
        TapLimitPerChange: Integer; // MaxTapChange

        InverseTime: Boolean; // IsInverseTime
        LDCActive: Boolean; // UseLineDrop
        IsReversible: Boolean; // UseReverseDrop

        TapDelay, // delay between taps // SubsequentDelay
        Vlimit, // VoltageLimit
        CTRating, // CT
        PTRatio, // PT
        Vreg, // TargetVoltage
        revVreg, // RevTargetVoltage
        Bandwidth, // BandVoltage
        revBandwidth, // RevBandVoltage
        R, // LineDropR
        X, // LineDropX
        revR, // RevLineDropR
        revX: Double; // RevLineDropX

        constructor Create(ParClass: TDSSClass; const RegControlName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        procedure RecalcElementData; OVERRIDE;
        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state
        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

        property Transformer: TTransfObj READ Get_Transformer;  // Pointer to controlled Transformer
        property TrWinding: Integer READ Get_Winding;  // Report Tapped winding
        property PendingTapChange: Double READ FPendingTapChange WRITE set_PendingTapChange;
        function VLimitActive: Boolean;
        // property InitialDelay: Double READ TimeDelay;
        property MinTap: Double READ Get_MinTap;
        property MaxTap: Double READ Get_MaxTap;
        property TapIncrement: Double READ Get_TapIncrement;
        property NumTaps: Integer READ Get_NumTaps;
        property TapNum: Integer READ Get_TapNum WRITE Set_TapNum;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    CktElement,
    Sysutils,
    uCmatrix,
    MathUtil,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TRegControlObj;
    TProp = TRegControlProp;

const
    NumPropsThisClass = Ord(High(TProp));

    // AVGPHASES = -1;
    MAXPHASE = -2;
    MINPHASE = -3;

    ACTION_TAPCHANGE = 0;
    ACTION_REVERSE = 1;
var
    PropInfo: Pointer;    
    PhaseEnum: TDSSEnum;

constructor TRegControl.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PhaseEnum := TDSSEnum.Create('RegControl: Phase Selection', True, 2, 2, 
            ['min', 'max'], [-3, -2]);
        PhaseEnum.Hybrid := True;
    end;

    Transf_Or_AutoTrans_ProxyClass := TProxyClass.Create(dssContext, ['Transformer', 'AutoTrans']);

    inherited Create(dssContext, REG_CONTROL, 'RegControl');
end;

destructor TRegControl.Destroy;
begin
    Transf_Or_AutoTrans_ProxyClass.Free;
    inherited Destroy;
end;

procedure SetTapNum(obj: TObj; Value: Integer);
begin
    obj.set_TapNum(Value);
end;

function GetTapNum(obj: TObj): Integer;
begin
    Result := obj.get_TapNum();
end;

procedure DoReset(Obj: TObj);
begin
    // force a reset
    Obj.Reset;
    //PropertyValue[29] := 'n'; // so it gets reported properly
end;

procedure TRegControl.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enum properties
    PropertyType[ord(TProp.PTphase)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.PTphase)] := ptruint(@obj.FPTPhase);
    PropertyOffset2[ord(TProp.PTphase)] := PtrInt(PhaseEnum);

    // object reference
    PropertyType[ord(TProp.transformer)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.transformer)] := ptruint(@obj.FControlledElement);
    PropertyOffset2[ord(TProp.transformer)] := ptruint(Transf_Or_AutoTrans_ProxyClass);
    PropertyWriteFunction[ord(TProp.transformer)] := @SetControlledElement;
    PropertyFlags[ord(TProp.transformer)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.CheckForVar];

    // double property, as Pascal property
    PropertyType[ord(TProp.TapNum)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.TapNum)] := 1;
    PropertyWriteFunction[ord(TProp.TapNum)] := @SetTapNum;
    PropertyReadFunction[ord(TProp.TapNum)] := @GetTapNum;
    PropertyFlags[ord(TProp.TapNum)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.ReadByFunction];

    // boolean properties
    PropertyType[ord(TProp.reversible)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.debugtrace)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.inversetime)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.revNeutral)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.EventLog)] := TPropertyType.BooleanProperty;
    PropertyType[ord(TProp.Cogen)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.reversible)] := ptruint(@obj.IsReversible);
    PropertyOffset[ord(TProp.debugtrace)] := ptruint(@obj.DebugTrace);
    PropertyOffset[ord(TProp.inversetime)] := ptruint(@obj.Inversetime);
    PropertyOffset[ord(TProp.revNeutral)] := ptruint(@obj.ReverseNeutral);
    PropertyOffset[ord(TProp.EventLog)] := ptruint(@obj.ShowEventLog);
    PropertyOffset[ord(TProp.Cogen)] := ptruint(@obj.CogenEnabled);

    // integer properties
    PropertyType[ord(TProp.winding)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.tapwinding)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.maxtapchange)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.winding)] := ptruint(@obj.ElementTerminal);
    PropertyOffset[ord(TProp.tapwinding)] := ptruint(@obj.TapWinding);
    PropertyOffset[ord(TProp.maxtapchange)] := ptruint(@obj.TapLimitPerChange);

    // string properties
    PropertyType[ord(TProp.bus)] := TPropertyType.StringProperty;
    PropertyOffset[ord(TProp.bus)] := ptruint(@obj.RegulatedBus);

    // double properties (default type)
    PropertyOffset[ord(TProp.vreg)] := ptruint(@obj.Vreg);
    PropertyOffset[ord(TProp.band)] := ptruint(@obj.Bandwidth);
    PropertyOffset[ord(TProp.ptratio)] := ptruint(@obj.PTRatio);
    PropertyOffset[ord(TProp.ctprim)] := ptruint(@obj.CTRating);
    PropertyOffset[ord(TProp.R)] := ptruint(@obj.R);
    PropertyOffset[ord(TProp.X)] := ptruint(@obj.X);
    PropertyOffset[ord(TProp.delay)] := ptruint(@obj.TimeDelay);
    PropertyOffset[ord(TProp.revvreg)] := ptruint(@obj.revVreg);
    PropertyOffset[ord(TProp.revband)] := ptruint(@obj.revBandwidth);
    PropertyOffset[ord(TProp.revR)] := ptruint(@obj.revR);
    PropertyOffset[ord(TProp.revX)] := ptruint(@obj.revX);
    PropertyOffset[ord(TProp.tapdelay)] := ptruint(@obj.TapDelay);
    PropertyOffset[ord(TProp.RemotePTRatio)] := ptruint(@obj.RemotePTRatio);
    PropertyOffset[ord(TProp.LDC_Z)] := ptruint(@obj.LDC_Z);
    PropertyOffset[ord(TProp.rev_Z)] := ptruint(@obj.revLDC_Z);
    PropertyOffset[ord(TProp.revThreshold)] := ptruint(@obj.kWRevPowerThreshold);
    PropertyOffset[ord(TProp.revDelay)] := ptruint(@obj.RevDelay);
    PropertyOffset[ord(TProp.Vlimit)] := ptruint(@obj.Vlimit);

    // boolean action
    PropertyType[ord(TProp.Reset)] := TPropertyType.BooleanActionProperty;
    PropertyOffset[ord(TProp.Reset)] := ptruint(@DoReset);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TRegControl.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TRegControlObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.Transformer):
        begin
            MonitoredElement := ControlledElement;  // same for this controller            
            PrpSequence[Idx] := -10;
        end;
        2:
            Tapwinding := ElementTerminal;  // Resets if property re-assigned
        5:
            RemotePTRatio := PTRatio;  // re-initialise RemotePTRatio whenever PTRatio is set
        ord(TProp.debugtrace):
            if DebugTrace then
            begin
                FreeAndNil(TraceFile);
                TraceFile := TFileStream.Create(DSS.OutputDirectory + 'REG_' + Name + '.csv', fmCreate);
                FSWriteln(TraceFile, 'Hour, Sec, ControlIteration, Iterations, LoadMultiplier, Present Tap, Pending Change, Actual Change, Increment, Min Tap, Max Tap');
                FSFlush(Tracefile);
            end
            else
            begin
                FreeAndNil(TraceFile);
            end;
        18:
            TapLimitPerChange := max(0, TapLimitPerChange);
        23:
            RevPowerThreshold := kWRevPowerThreshold * 1000.0;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TRegControlObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNphases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    ControlledElement := Other.ControlledElement;  // Pointer to target circuit element
    ElementTerminal := Other.ElementTerminal;

    Vreg := Other.Vreg;
    Bandwidth := Other.Bandwidth;
    PTRatio := Other.PTRatio;
    RemotePTRatio := Other.RemotePTRatio;
    CTRating := Other.CTRating;
    R := Other.R;
    X := Other.X;
    RegulatedBus := Other.RegulatedBus;
    TimeDelay := Other.TimeDelay;
    IsReversible := Other.IsReversible;
    revVreg := Other.revVreg;
    revBandwidth := Other.revBandwidth;
    revR := Other.revR;
    revX := Other.revX;
    TapDelay := Other.TapDelay;
    TapWinding := Other.TapWinding;
    Inversetime := Other.Inversetime;

    TapLimitPerChange := Other.TapLimitPerChange;
    kWRevPowerThreshold := Other.kWRevPowerThreshold;
    RevPowerThreshold := Other.RevPowerThreshold;
    RevDelay := Other.RevDelay;
    ReverseNeutral := Other.ReverseNeutral;
    ShowEventLog := Other.ShowEventLog;
    // DebugTrace := Other.DebugTrace;  Always default to NO

    FPTphase := Other.FPTphase;
    TapNum := Other.TapNum;
    CogenEnabled := Other.CogenEnabled;
    LDC_Z := Other.LDC_Z;
    RevLDC_Z := Other.revLDC_Z;
end;

constructor TRegControlObj.Create(ParClass: TDSSClass; const RegControlName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(RegControlName);
    DSSObjType := ParClass.DSSClassType;
    TraceFile := nil;

    LastChange := 0;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class

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

    ControlledElement := NIL;
    ElementTerminal := 1;
    TapWinding := ElementTerminal;

    VBuffer := NIL;
    CBuffer := NIL;

    DSSObjType := ParClass.DSSClassType; //REG_CONTROL;

    Inversetime := FALSE;
    RegulatedBus := '';
    Vlimit := 0.0;

    ControlActionHandle := 0;

   //  RecalcElementData;
end;

destructor TRegControlObj.Destroy;
begin
    if Assigned(VBuffer) then
        ReallocMem(VBuffer, 0);
    if Assigned(CBuffer) then
        ReallocMem(CBuffer, 0);

    FreeAndNil(TraceFile);

    inherited Destroy;
end;

procedure TRegControlObj.RecalcElementData;
var
    ename: String;
begin
    if (R <> 0.0) or (X <> 0.0) or (LDC_Z > 0.0) then
        LDCActive := TRUE
    else
        LDCActive := FALSE;
    if Length(RegulatedBus) = 0 then
        UsingRegulatedBus := FALSE
    else
        UsingRegulatedBus := TRUE;

    if ControlledElement = NIL then
    begin
        // element not found or not set
        DoErrorMsg(
            Format(_('RegControl: "%s"'), [Self.Name]),
            _('Transformer Element is not set.'),
            _('Element must be defined previously.'), 124);
        Exit;
    end;

    if UsingRegulatedBus then
    begin
        FNphases := 1;     // Only need one phase
        Nconds := 2;
    end
    else
    begin
        FNphases := ControlledElement.NPhases;
        Nconds := FNphases;
        if FPTphase > FNphases then
        begin
            FPTphase := 1;
            SetAsNextSeq(ord(TProp.PTphase));
        end;
    end;

    if (Comparetext(ControlledElement.DSSClassName, 'transformer') = 0) or  // either should work
        (Comparetext(ControlledElement.DSSClassName, 'autotrans') = 0) then
    begin
        if ElementTerminal > ControlledElement.Nterms then
        begin
            DoErrorMsg(
                Format(_('RegControl: "%s"'), [Name]),
                Format(_('Winding no. "%d" does not exist.'), [ElementTerminal]),
                _('Respecify Monitored Winding no.'), 122);
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
        ename := ControlledElement.Name;
        ControlledElement := NIL;   
        DoErrorMsg(
            Format(_('RegControl: "%s"'), [Self.Name]),
            Format(_('Controlled Regulator Element "%s" is not a transformer.'), [ename]),
            _('Element must be defined previously.'), 123);
    end;
end;

function TRegControlObj.GetControlVoltage(VBuffer: pComplexArray; Nphs: Integer; PTRatio: Double): Complex;
var
    i: Integer;
    V: Double;
begin
    case FPTphase of
//         AVGPHASES: Begin
//                        Result := CZERO;
//                        FOR i := 1 to Nphs Do Result := Result + Cabs(VBuffer^[i]);
//                        Result := Result / (Nphs*PTRatio);
//                    End;
//
        MAXPHASE:
        begin
            ControlledPhase := 1;
            V := Cabs(VBuffer^[ControlledPhase]);
            for i := 2 to Nphs do
                if Cabs(VBuffer^[i]) > V then
                begin
                    V := Cabs(VBuffer^[i]);
                    ControlledPhase := i;
                end;
            Result := VBuffer^[ControlledPhase] / PTRatio;
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
            Result := VBuffer^[ControlledPhase] / PTRatio;
        end;
    else
    {Just use one phase because that's what most controls do.}
        Result := VBuffer^[FPTPhase] / PTRatio;
        ControlledPhase := FPTPhase;
    end;
end;

procedure TRegControlObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;

    if Complete then
    begin
        FSWriteln(F, '! Bus =' + GetBus(1));
        FSWriteln(F);
    end;
end;

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

function OneInDirectionOf(Obj: TObj; var ProposedChange: Double; Increment: Double): Double;
// Computes the amount of one tap change in the direction of the pending tapchange
// Automatically decrements the proposed change by that amount
begin
    Obj.LastChange := 0;
    if ProposedChange > 0.0 then
    begin
        Result := Increment;
        Obj.LastChange := 1;
        ProposedChange := ProposedChange - Increment;
    end
    else
    begin
        Result := -Increment;
        Obj.LastChange := -1;
        ProposedChange := ProposedChange + Increment;
    end;

    if Abs(ProposedChange) < 0.9 * Increment then
        ProposedChange := 0.0;
end;

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
                with TTransfObj(ControlledElement) do
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
                                TapChangeToMake := OneInDirectionOf(Self, FPendingTapChange, TapIncrement[TapWinding]);
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
                                TapChangeToMake := OneInDirectionOf(Self, FPendingTapChange, TapIncrement[TapWinding]);
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
                                TapChangeToMake := OneInDirectionOf(Self, FPendingTapChange, TapIncrement[TapWinding]);
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
                RegWriteDebugRecord(Format('%-.6g, Handling Reverse Action, ReversePending=%s, InReverseMode=%s',
                    [ActiveCircuit.Solution.dynavars.dblHour, BoolToStr(ReversePending, TRUE), BoolToStr(InReverseMode, TRUE)]));
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
        if (DebugTrace) then 
        with ActiveCircuit do
            RegWriteDebugRecord(Format(
                '%-.6g, 2-Looking forward= %s *** Incogenmode=%s',
                [ActiveCircuit.Solution.DynaVars.dblHour, BoolToStr(LookingForward, TRUE), BoolToStr(InCogenMode, TRUE)]
            ));
    
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
                            RevHandle := ControlQueue.Push(ActiveCircuit.Solution.DynaVars.intHour, ActiveCircuit.Solution.DynaVars.t + RevDelay, ACTION_REVERSE, 0, Self);
                        if (DebugTrace) then
                            RegWriteDebugRecord(Format('%-.6g, 1- Pushed Reverse Action, Handle=%d, FwdPower=%.8g', 
                                [ActiveCircuit.Solution.DynaVars.dblHour, RevHandle, FwdPower]
                            ));
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
                if (DebugTrace) then 
                    with ActiveCircuit do
                        RegWriteDebugRecord(Format(
                            '%-.6g, 3-Looking Forward=%s *** Incogenmode=%s',
                            [ActiveCircuit.Solution.DynaVars.dblHour, BoolToStr(Lookingforward, TRUE), BoolToStr(InCogenMode, TRUE)]
                        ));

                FwdPower := -ControlledTransformer.Power[ElementTerminal].re;  // watts
                if not ReversePending then
                begin
                    if (FwdPower > RevPowerThreshold) then
                    begin
                        ReversePending := TRUE;
                        with ActiveCircuit do
                            RevBackHandle := ControlQueue.Push(ActiveCircuit.Solution.DynaVars.intHour, ActiveCircuit.Solution.DynaVars.t + RevDelay, ACTION_REVERSE, 0, Self);
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
                  {Both Cogen Mode and Reverse operation}
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
                                            RegWriteDebugRecord(Format('*** %.6g s: Pushing TapChange = %.8g, delay= %.8g', [ActiveCircuit.Solution.DynaVars.t, PendingTapChange, TapDelay]));
                                        ControlQueue.Push(ActiveCircuit.Solution.DynaVars.intHour, ActiveCircuit.Solution.DynaVars.t + TapDelay, ACTION_TAPCHANGE, 0, Self);
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
                    VBuffer^[i] := Vterminal^[i] - Vterminal^[ii];
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
            Vlocalbus := Cabs(VBuffer^[1] / PTRatio);
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
        ILDC := (CBuffer^[ControlledElement.Nconds * (ElementTerminal - 1) + ControlledPhase]) / CTRating;
        if LDC_Z = 0.0 then  // Standard R, X LDC
        begin
            if InReverseMode or InCogenMode then
                VLDC := Cmplx(revR, revX) * ILDC
            else
                VLDC := Cmplx(R, X) *  ILDC;
            Vcontrol := Vcontrol + VLDC;   // Direction on ILDC is INTO terminal, so this is equivalent to Vterm - (R+jX)*ILDC
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
            if inCogenMode then
            begin
                VregTest := RevVreg;    // corrected Feb 25, 2021 for Huijuan Li
                BandTest := RevBandwidth;
            end
            else
            begin
                VregTest := Vreg;
                BandTest := Bandwidth;
            end;        
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
        if (not DSS.InShowResults) then
        begin
            FSWriteln(TraceFile, S);
            FSFlush(TraceFile);
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
    sout: String;
begin
    try
        if (not DSS.InShowResults) then
        begin
            Separator := ', ';
            with TTransfObj(ControlledElement) do
            begin
                WriteStr(sout,
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

                FSWriteln(TraceFile, sout);
            end;
            FSFlush(TraceFile);
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


procedure TRegControlObj.MakePosSequence();
begin
    if ControlledElement <> NIL then
    begin
        Enabled := ControlledElement.Enabled;
        if UsingRegulatedBus then
            FNphases := 1
        else
            FNphases := ControlledElement.NPhases;
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
    if inversetime then
        Result := TimeDelay / Min(10.0, (2.0 * Abs(Vreg - Vavg) / Bandwidth))
    else
        Result := TimeDelay;
end;

procedure TRegControlObj.Set_Enabled(Value: Boolean);
begin
    // Do nothing else besides toggling the flag,
    // we don't need BusNameRedefined from CktElement.pas
    FEnabled := Value;
end;

function TRegControlObj.VLimitActive: Boolean;
begin
    Result := VLimit > 0.0;
end;

initialization
    PropInfo := NIL;
finalization
    PhaseEnum.Free;
end.
