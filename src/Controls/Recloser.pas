unit Recloser;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Classes,
    Command,
    ControlClass,
    ControlElem,
    CktElement,
    DSSClass,
    Arraydef,
    UComplex, DSSUcomplex,
    utilities,
    TCC_Curve,
    Math;

type
{$SCOPEDENUMS ON}
    TRecloserProp = (
        INVALID = 0,
        MonitoredObj = 1,
        MonitoredTerm = 2,
        SwitchedObj = 3,
        SwitchedTerm = 4,
        NumFast = 5,
        PhaseFast = 6,
        PhaseDelayed = 7,
        GroundFast = 8,
        GroundDelayed = 9,
        PhaseTrip = 10,
        GroundTrip = 11,
        PhaseInst = 12,
        GroundInst = 13,
        Reset = 14,
        Shots = 15, 
        RecloseIntervals = 16,
        Delay = 17,
        Action = 18, 
        TDPhFast = 19,
        TDGrFast = 20,
        TDPhDelayed = 21,
        TDGrDelayed = 22,
        Normal = 23, 
        State = 24
    );
{$SCOPEDENUMS OFF}

    TRecloser = class(TControlClass)
    PROTECTED
        TCC_CurveClass: TDSSClass;

        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TRecloserObj = class(TControlElem)
    PRIVATE
        PhaseDelayed,
        GroundDelayed,
        PhaseFast,
        GroundFast: TTCC_CurveObj;

        ResetTime,
        DelayTime,
        TDGrDelayed,
        TDPhDelayed,
        TDGrFast,
        TDPhFast: Double;

        FPresentState: EControlAction;

        OperationCount: Integer;

        LockedOut,
        ArmedForClose,
        ArmedForOpen,
        GroundTarget,
        PhaseTarget: Boolean;

        CondOffset: Integer; // Offset for monitored terminal

        cBuffer: pComplexArray;    // Complexarray buffer

        function  get_PresentState: EControlAction; //TODO: check why this function even exists (also in Relay)
        procedure set_PresentState(const Value: EControlAction);
    PUBLIC
        NormalState: EControlAction;
        NormalStateSet: Boolean;
        RecloseIntervals: pdoubleArray; //TODO: make a fixed-size array
        NumFast,
        NumReclose: Integer;
        MonitoredElementTerminal: Integer;
        PhaseTrip,
        GroundTrip,
        PhaseInst,
        GroundInst: Double;

        constructor Create(ParClass: TDSSClass; const RecloserName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        procedure RecalcElementData; OVERRIDE;

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        property PresentState: EControlAction read get_PresentState write set_PresentState;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Circuit,
    Sysutils,
    uCmatrix,
    MathUtil,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TRecloserObj;
    TProp = TRecloserProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    
    ActionEnum, StateEnum: TDSSEnum;

constructor TRecloser.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ActionEnum := TDSSEnum.Create('Recloser: Action', False, 1, 1, 
            ['close', 'open', 'trip'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN), ord(CTRL_OPEN)]);
        StateEnum := TDSSEnum.Create('Recloser: State', False, 1, 1, 
            ['closed', 'open', 'trip'], 
            [ord(CTRL_CLOSE), ord(CTRL_OPEN), ord(CTRL_OPEN)]);
    end;

    TCC_CurveClass := GetDSSClassPtr(dssContext, 'TCC_Curve');

    inherited Create(dssContext, RECLOSER_CONTROL, 'Recloser');
end;

destructor TRecloser.Destroy;
begin
    inherited Destroy;
end;

procedure TRecloser.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enums
    PropertyType[ord(TProp.Action)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@obj.FPresentState);
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum);
    PropertyFlags[ord(TProp.Action)] := [TPropertyFlag.Redundant];

    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.State)] := ptruint(@obj.FPresentState);
    PropertyOffset2[ord(TProp.State)] := PtrInt(StateEnum);

    PropertyType[ord(TProp.Normal)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Normal)] := ptruint(@obj.NormalState);
    PropertyOffset2[ord(TProp.Normal)] := PtrInt(StateEnum);

    // double arrays/vectors
    PropertyType[ord(TProp.RecloseIntervals)] := TPropertyType.DoubleVArrayProperty;
    PropertyOffset[ord(TProp.RecloseIntervals)] := ptruint(@obj.RecloseIntervals);
    PropertyOffset2[ord(TProp.RecloseIntervals)] := ptruint(@obj.NumReclose);
    PropertyOffset3[ord(TProp.RecloseIntervals)] := 4;
    PropertyFlags[ord(TProp.RecloseIntervals)] := [TPropertyFlag.ArrayMaxSize];

    // object properties
    PropertyType[ord(TProp.PhaseFast)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.PhaseFast)] := ptruint(@obj.PhaseFast);
    PropertyOffset2[ord(TProp.PhaseFast)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.PhaseDelayed)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.PhaseDelayed)] := ptruint(@obj.PhaseDelayed);
    PropertyOffset2[ord(TProp.PhaseDelayed)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.GroundFast)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.GroundFast)] := ptruint(@obj.GroundFast);
    PropertyOffset2[ord(TProp.GroundFast)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.GroundDelayed)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.GroundDelayed)] := ptruint(@obj.GroundDelayed);
    PropertyOffset2[ord(TProp.GroundDelayed)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.MonitoredObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.MonitoredObj)] := ptruint(@obj.FMonitoredElement);
    PropertyOffset2[ord(TProp.MonitoredObj)] := 0;
    PropertyWriteFunction[ord(TProp.MonitoredObj)] := @SetMonitoredElement;
    PropertyFlags[ord(TProp.MonitoredObj)] := [TPropertyFlag.WriteByFunction];
    //[TPropertyFlag.CheckForVar]; // not required for general cktelements

    PropertyType[ord(TProp.SwitchedObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.SwitchedObj)] := ptruint(@obj.FControlledElement);
    PropertyOffset2[ord(TProp.SwitchedObj)] := 0;
    PropertyWriteFunction[ord(TProp.SwitchedObj)] := @SetControlledElement;
    PropertyFlags[ord(TProp.SwitchedObj)] := [TPropertyFlag.WriteByFunction]; 
    //[TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.SwitchedTerm)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.MonitoredTerm)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.NumFast)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.SwitchedTerm)] := ptruint(@obj.ElementTerminal);
    PropertyOffset[ord(TProp.MonitoredTerm)] := ptruint(@obj.MonitoredElementTerminal);
    PropertyOffset[ord(TProp.NumFast)] := ptruint(@obj.NumFast);

    PropertyType[ord(TProp.Shots)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Shots)] := ptruint(@obj.NumReclose);
    PropertyFlags[ord(TProp.Shots)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.ValueOffset];
    PropertyValueOffset[ord(TProp.Shots)] := -1;

    // double properties (default type)
    PropertyOffset[ord(TProp.PhaseTrip)] := ptruint(@obj.PhaseTrip);
    PropertyOffset[ord(TProp.GroundTrip)] := ptruint(@obj.GroundTrip);
    PropertyOffset[ord(TProp.PhaseInst)] := ptruint(@obj.PhaseInst);
    PropertyOffset[ord(TProp.GroundInst)] := ptruint(@obj.GroundInst);
    PropertyOffset[ord(TProp.Reset)] := ptruint(@obj.Resettime);
    PropertyOffset[ord(TProp.Delay)] := ptruint(@obj.DelayTime);
    PropertyOffset[ord(TProp.TDPhFast)] := ptruint(@obj.TDPhFast);
    PropertyOffset[ord(TProp.TDGrFast)] := ptruint(@obj.TDGrFast);
    PropertyOffset[ord(TProp.TDPhDelayed)] := ptruint(@obj.TDPhDelayed);
    PropertyOffset[ord(TProp.TDGrDelayed)] := ptruint(@obj.TDGrDelayed);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TRecloser.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TRecloserObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.MonitoredObj):
            // Default the controlled element to the monitored element
            ControlledElement := MonitoredElement; //TODO: This can cause unexpected behavior
        ord(TProp.MonitoredTerm):
            ElementTerminal := MonitoredElementTerminal; //TODO: This can cause unexpected behavior
        ord(TProp.Normal):
            NormalStateSet := true;
        ord(TProp.Action), ord(TProp.State):
            if not NormalStateSet then
            begin
                NormalStateSet := true;
                NormalState := FPresentState;
            end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TRecloserObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    ElementTerminal := Other.ElementTerminal;
    ControlledElement := Other.ControlledElement;  // Pointer to target circuit element

    MonitoredElement := Other.MonitoredElement;  // Pointer to target circuit element
    MonitoredElementTerminal := Other.MonitoredElementTerminal;  // Pointer to target circuit element

    PhaseDelayed := Other.PhaseDelayed;
    GroundDelayed := Other.GroundDelayed;
    PhaseFast := Other.PhaseFast;
    GroundFast := Other.GroundFast;
    PhaseTrip := Other.PhaseTrip;
    GroundTrip := Other.GroundTrip;
    PhaseInst := Other.PhaseInst;
    GroundInst := Other.GroundInst;
    Resettime := Other.Resettime;
    NumReclose := Other.NumReclose;
    NumFast := Other.NumFast;

    Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4);      // Always make a max of 4
    for i := 1 to NumReclose do
        RecloseIntervals^[i] := Other.RecloseIntervals^[i];

    LockedOut := Other.LockedOut;
    FPresentState := Other.FPresentState;
    NormalState := Other.NormalState;
    NormalStateSet := Other.NormalStateSet;
    CondOffset := Other.CondOffset;
end;

constructor TRecloserObj.Create(ParClass: TDSSClass; const RecloserName: String);
begin
    inherited Create(ParClass);
    Name := LowerCase(RecloserName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class

    ControlledElement := NIL;
    ElementTerminal := 1;

    MonitoredElementTerminal := 1;
    MonitoredElement := NIL;

    PhaseFast := TRecloser(ParClass).TCC_CurveClass.Find('a'); //TODO: is an error message ever required for this?
    PhaseDelayed := TRecloser(ParClass).TCC_CurveClass.Find('d'); //TODO: is an error message ever required for this?
    GroundFast := NIL;
    GroundDelayed := NIL;

    PhaseTrip := 1.0;
    GroundTrip := 1.0;
    PhaseInst := 0.0;
    GroundInst := 0.0;

    TDGrDelayed := 1.0;
    TDPhDelayed := 1.0;
    TDGrFast := 1.0;
    TDPhFast := 1.0;

    Resettime := 15.0;
    NumReclose := 3;
    NumFast := 1;

    RecloseIntervals := NIL;
    Reallocmem(RecloseIntervals, SizeOf(RecloseIntervals^[1]) * 4); // fixed allocation of 4
    RecloseIntervals^[1] := 0.5;
    RecloseIntervals^[2] := 2.0;
    RecloseIntervals^[3] := 2.0;

    FPresentState := CTRL_CLOSE;
    NormalState := CTRL_CLOSE;
    NormalStateSet := FALSE;

    Operationcount := 1;
    LockedOut := FALSE;
    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    GroundTarget := FALSE;
    PhaseTarget := FALSE;

    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

   //  RecalcElementData;
end;

destructor TRecloserObj.Destroy;
begin
    ReallocMem(RecloseIntervals, 0);
    ReallocMem(cBuffer, 0);
    inherited Destroy;
end;

procedure TRecloserObj.RecalcElementData;
begin
    //TODO: still need to warn/error if elements are NIL?

    if MonitoredElement <> NIL then
    begin
        FNphases := MonitoredElement.NPhases;       // Force number of phases to be same
        if MonitoredElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg(Format(_('Recloser: "%s"'), [Name]),
                Format(_('Terminal no. "%d" does not exist.'), [MonitoredElementTerminal]),
                _('Re-specify terminal no.'), 392);
        end
        else
        begin
               // Sets name of i-th terminal's connected bus in Recloser's buslist
            Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
               // Allocate a buffer bigenough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
        end;
    end;

    // Check for existence of Controlled Element

    // If previously assigned, reset HasOCPDevice flag in case this is a move
    if ControlledElement <> NIL then
    begin
        Exclude(ControlledElement.Flags, Flg.HasOCPDevice);
        Exclude(ControlledElement.Flags, Flg.HasAutoOCPDevice);
    
        // Both CktElement and monitored element must already exist
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

        // If the recloser becomes disabled, leave at False
        if Enabled then
        begin 
            Include(ControlledElement.Flags, Flg.HasOCPDevice); // For Reliability calcs
            Include(ControlledElement.Flags, Flg.HasAutoOCPDevice); // For Reliability calcs
        end;

        // Open/Close State of controlled element based on state assigned to the control
        if FPresentState = CTRL_CLOSE then
        begin
            ControlledElement.Closed[0] := TRUE;
            LockedOut := FALSE;
            OperationCount := 1;
            ArmedForOpen := FALSE;
        end
        else
        begin
            ControlledElement.Closed[0] := FALSE;
            LockedOut := TRUE;
            OperationCount := NumReclose + 1;
            ArmedForClose := FALSE;
        end;
    end;
end;

procedure TRecloserObj.MakePosSequence();
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := MonitoredElement.NPhases;
        Nconds := FNphases;
        Setbus(1, MonitoredElement.GetBus(ElementTerminal));
        // Allocate a buffer bigenough to hold everything from the monitored element
        ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
        CondOffset := (ElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
    end;
    inherited;
end;

procedure TRecloserObj.DoPendingAction(const Code, ProxyHdl: Integer);
begin
    with ControlledElement do
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal of CktElement to terminal 1
        case Code of
            Integer(CTRL_OPEN):
                case FPresentState of
                    CTRL_CLOSE:
                        if ArmedForOpen then
                        begin   // ignore if we became disarmed in meantime
                            ControlledElement.Closed[0] := FALSE;   // Open all phases of active terminal
                            if OperationCount > NumReclose then
                            begin
                                LockedOut := TRUE;
                                AppendtoEventLog('Recloser.' + Self.Name, 'Opened, Locked Out');
                            end
                            else
                            begin
                                if OperationCount > NumFast then
                                    AppendtoEventLog('Recloser.' + Self.Name, 'Opened, Delayed')
                                else
                                    AppendtoEventLog('Recloser.' + Self.Name, 'Opened, Fast');
                            end;
                            if PhaseTarget then
                                AppendtoEventLog(' ', 'Phase Target');
                            if GroundTarget then
                                AppendtoEventLog(' ', 'Ground Target');
                            ArmedForOpen := FALSE;
                        end;
                else // Nada
                end;
            Integer(CTRL_CLOSE):
                case FPresentState of
                    CTRL_OPEN:
                        if ArmedForClose and not LockedOut then
                        begin
                            ControlledElement.Closed[0] := TRUE; // Close all phases of active terminal
                            Inc(OperationCount);
                            AppendtoEventLog('Recloser.' + Self.Name, 'Closed');
                            ArmedForClose := FALSE;
                        end;
                else // Nada
                end;
            Integer(CTRL_RESET):
                case FPresentState of
                    CTRL_CLOSE:
                        if not ArmedForOpen then
                            OperationCount := 1; // Don't reset if we just rearmed
                else  // Nada
                end;
        else
            // Do Nothing
        end;

    end;
end;

procedure TRecloserObj.Sample;
var
    i: Integer;
    cmag: Double;
    Csum: Complex;

    GroundCurve, PhaseCurve: TTCC_CurveObj;
    Groundtime, PhaseTime, TripTime, TimeTest: Double;
    TDPhase, TDGround: Double;
begin
    ControlledElement.ActiveTerminalIdx := ElementTerminal;

    if ControlledElement.Closed[0] // Check state of phases of active terminal
    then
        FPresentState := CTRL_CLOSE
    else
        FPresentState := CTRL_OPEN;

    with MonitoredElement do
    begin
        if OperationCount > NumFast then
        begin
            GroundCurve := GroundDelayed;
            PhaseCurve := PhaseDelayed;
            TDGround := TDGrDelayed;
            TDPhase := TDPhDelayed;
        end
        else
        begin
            GroundCurve := GroundFast;
            PhaseCurve := PhaseFast;
            TDGround := TDGrFast;
            TDPhase := TDPhFast;
        end;

        if FPresentState = CTRL_CLOSE then
        begin
            TripTime := -1.0;
            GroundTime := -1.0;
            PhaseTime := -1.0;  {No trip}

            // Check largest Current of all phases of monitored element
            MonitoredElement.GetCurrents(cBuffer);

            // Check Ground Trip, if any
            if GroundCurve <> NIL then
            begin
                Csum := CZERO;
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                    Csum += cBuffer^[i];
                Cmag := Cabs(Csum);
                if (GroundInst > 0.0) and (Cmag >= GroundInst) and (OperationCount = 1) then
                    GroundTime := 0.01 + DelayTime      // Inst trip on first operation
                else
                    GroundTime := TDGround * GroundCurve.GetTCCTime(Cmag / GroundTrip);
            end;

            if Groundtime > 0.0 then
            begin
                TripTime := GroundTime;
                GroundTarget := TRUE;
            end;

            // If GroundTime > 0 then we have a ground trip

            // Check Phase Trip, if any

            if PhaseCurve <> NIL then
            begin
                for i := (1 + CondOffset) to (Fnphases + CondOffset) do
                begin
                    Cmag := Cabs(cBuffer^[i]);


                    if (PhaseInst > 0.0) and (Cmag >= PhaseInst) and (OperationCount = 1) then
                    begin
                        PhaseTime := 0.01 + DelayTime;  // Inst trip on first operation
                        Break;  {FOR - if Inst, no sense checking other phases}
                    end
                    else
                    begin
                        TimeTest := TDPhase * PhaseCurve.GetTCCTime(Cmag / PhaseTrip);
                        if (TimeTest > 0.0) then
                        begin
                            if Phasetime < 0.0 then
                                PhaseTime := TimeTest
                            else
                                PhaseTime := Min(PhaseTime, TimeTest);
                        end;
                    end;

                end;
            end;
               // If PhaseTime > 0 then we have a phase trip

            if PhaseTime > 0.0 then
            begin
                PhaseTarget := TRUE;
                if TripTime > 0.0 then
                    TripTime := Min(TripTime, Phasetime)
                else
                    TripTime := PhaseTime;
            end;

            if TripTime > 0.0 then
            begin
                if not ArmedForOpen then
                    with ActiveCircuit do   // Then arm for an open operation
                    begin
                        ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, CTRL_OPEN, 0, Self);
                        if OperationCount <= NumReclose then
                            ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + DelayTime + RecloseIntervals^[OperationCount], CTRL_CLOSE, 0, Self);
                        ArmedForOpen := TRUE;
                        ArmedForClose := TRUE;
                    end;
            end
            else
            begin
                if ArmedForOpen then
                    with ActiveCircuit do    // If current dropped below pickup, disarm trip and set for reset
                    begin
                        ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + ResetTime, CTRL_RESET, 0, Self);
                        ArmedForOpen := FALSE;
                        ArmedForClose := FALSE;
                        GroundTarget := FALSE;
                        PhaseTarget := FALSE;
                    end;
            end;
        end;  // IF PresentState=CLOSE
    end; // With
end;

procedure TRecloserObj.Reset;
begin
    FPresentState := NormalState;
    ArmedForOpen := FALSE;
    ArmedForClose := FALSE;
    GroundTarget := FALSE;
    PhaseTarget := FALSE;

    if ControlledElement = NIL then
        Exit;

    ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Set active terminal

    if NormalState = CTRL_OPEN then
    begin
        ControlledElement.Closed[0] := FALSE; // Open all phases of active terminal
        LockedOut := TRUE;
        OperationCount := NumReclose + 1;
    end
    else
    begin
        ControlledElement.Closed[0] := TRUE; // Close all phases of active terminal
        LockedOut := FALSE;
        Operationcount := 1;
    end;
end;

function TRecloserObj.get_PresentState: EControlAction; //TODO: why GetPropertyValue doesn't use this one?
begin
    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;
        if ControlledElement.Closed[0] then
            FPresentState := CTRL_CLOSE
        else
            FPresentState := CTRL_OPEN;
    end;

    Result := FPresentState;
End;

Procedure TRecloserObj.set_PresentState(const Value: EControlAction);
Begin
    if PresentState = Value then 
        Exit;

    FPresentState := Value;

    if ControlledElement = NIL then
        Exit;

    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    if Value = CTRL_OPEN then
    begin
        ControlledElement.Closed[0] := FALSE;
        LockedOut := TRUE;
        OperationCount := NumReclose + 1;
        ArmedForClose := FALSE;
    end
    else 
    {if Value = CTRL_CLOSE then} 
    begin
        ControlledElement.Closed[0] := TRUE;
        LockedOut := FALSE;
        OperationCount := 1;
        ArmedForOpen := FALSE;
    end;
end;

initialization
    PropInfo := NIL;
finalization        
    ActionEnum.Free;
    StateEnum.Free;
end.
