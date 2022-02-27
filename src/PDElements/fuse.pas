unit Fuse;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
//  A Fuse is a control element that is connected to a terminal of a
//  circuit element and controls the switches in the same or another terminal.
//
//  The control is usually placed in the
//  terminal of a line or transformer, but it could be any element
//
//  CktElement to be controlled must already exist.

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

const
    FUSEMAXDIM = 6;

type
{$SCOPEDENUMS ON}
    TFuseProp = (
        INVALID = 0,
        MonitoredObj = 1,
        MonitoredTerm = 2,
        SwitchedObj = 3,
        SwitchedTerm = 4,
        FuseCurve = 5,
        RatedCurrent = 6,
        Delay = 7,
        Action = 8,
        Normal = 9,
        State = 10
    );
{$SCOPEDENUMS OFF}

    pStateArray = ^StateArray;
    StateArray = array[1..FUSEMAXDIM] of EControlAction;

    TFuse = class(TControlClass)
    PRIVATE
        TCC_CurveClass: TDSSClass;
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TFuseObj = class(TControlElem)
    PRIVATE
        PreviousControlledElement: TDSSCktElement;
        MonitoredElement: TDSSCktElement; // TODO: check why this exists (TControlElem already has one)

        hAction: array[1..FUSEMAXDIM] of Integer;  // handle to control queue actions
        ReadyToBlow: array[1..FUSEMAXDIM] of Boolean;
        CondOffset: Integer; // Offset for monitored terminal
        cBuffer: pComplexArray; // Complexarray buffer

        function get_States(Idx: Integer): EControlAction;
    PUBLIC
        FuseCurve: TTCC_CurveObj;
        RatedCurrent: Double;
        DelayTime: Double;

        MonitoredElementTerminal: Integer;
        FPresentState, FNormalState: pStateArray;
        NormalStateSet: Boolean;

        constructor Create(ParClass: TDSSClass; const FuseName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;    // Always Zero for a Fuse

        procedure Sample; OVERRIDE;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Phs, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Get present value of terminal Curr

        function GetPropertyValue(Index: Integer): String; OVERRIDE;

        property States[Idx: Integer]: EControlAction read get_States;
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
    TObj = TFuseObj;
    TProp = TFuseProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    
    ActionEnum, StateEnum: TDSSEnum;

constructor TFuse.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ActionEnum := TDSSEnum.Create('Fuse: Action', False, 1, 1, 
            ['close', 'open'], [ord(CTRL_CLOSE), ord(CTRL_OPEN)]);
        StateEnum := TDSSEnum.Create('Fuse: State', False, 1, 1, 
            ['closed', 'open'], [ord(CTRL_CLOSE), ord(CTRL_OPEN)]);
    end;

    TCC_CurveClass := GetDSSClassPtr(dssContext, 'TCC_Curve');
    inherited Create(dssContext, FUSE_CONTROL, 'Fuse');
end;

destructor TFuse.Destroy;
begin
    inherited Destroy;
end;

procedure DoAction(obj: TObj; action: EControlAction);
var
    i: Integer;
begin
    case action of 
        CTRL_OPEN:
            for i := 1 to FUSEMAXDIM do
                Obj.FPresentState[i] := CTRL_OPEN;
        CTRL_CLOSE:
            for i := 1 to FUSEMAXDIM do
                Obj.FPresentState[i] := CTRL_CLOSE;
    end;
    Obj.PropertySideEffects(ord(TProp.State));
end;

function GetFuseStateSize(Obj: TObj): Integer;
begin
    Result := FUSEMAXDIM;
    if Obj.ControlledElement <> NIL then
        Result := Obj.ControlledElement.NPhases;
end;

procedure TFuse.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // object properties
    PropertyType[ord(TProp.FuseCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.FuseCurve)] := ptruint(@obj.FuseCurve);
    PropertyOffset2[ord(TProp.FuseCurve)] := ptruint(TCC_CurveClass);

    PropertyType[ord(TProp.MonitoredObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.MonitoredObj)] := ptruint(@obj.MonitoredElement);
    //PropertyWriteFunction[ord(TProp.MonitoredObj)] := @SetMonitoredElement;
    //PropertyFlags[ord(TProp.MonitoredObj)] := [TPropertyFlag.WriteByFunction];//[TPropertyFlag.CheckForVar]; // not required for general cktelements

    PropertyType[ord(TProp.SwitchedObj)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.SwitchedObj)] := ptruint(@obj.FControlledElement);
    PropertyWriteFunction[ord(TProp.SwitchedObj)] := @SetControlledElement;
    PropertyFlags[ord(TProp.SwitchedObj)] := [TPropertyFlag.WriteByFunction];// [TPropertyFlag.CheckForVar]; // not required for general cktelements

    // integer properties
    PropertyType[ord(TProp.MonitoredTerm)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.SwitchedTerm)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.MonitoredTerm)] := ptruint(@obj.MonitoredElementTerminal);
    PropertyOffset[ord(TProp.SwitchedTerm)] := ptruint(@obj.ElementTerminal);

    // double properties
    PropertyOffset[ord(TProp.RatedCurrent)] := ptruint(@obj.RatedCurrent);
    PropertyOffset[ord(TProp.Delay)] := ptruint(@obj.DelayTime);

    // enum action
    PropertyType[ord(TProp.Action)] := TPropertyType.StringEnumActionProperty;
    PropertyOffset[ord(TProp.Action)] := ptruint(@DoAction); 
    PropertyOffset2[ord(TProp.Action)] := PtrInt(ActionEnum);

    PropertyType[ord(TProp.Normal)] := TPropertyType.MappedStringEnumArrayProperty;
    PropertyOffset[ord(TProp.Normal)] := ptrint(@obj.FNormalState); 
    PropertyOffset2[ord(TProp.Normal)] := ptrint(StateEnum); 
    PropertyOffset3[ord(TProp.Normal)] := ptrint(@GetFuseStateSize);
    PropertyFlags[ord(TProp.Normal)] := [TPropertyFlag.SizeIsFunction]; // FControlledElement.NPhases

    PropertyType[ord(TProp.State)] := TPropertyType.MappedStringEnumArrayProperty;
    PropertyOffset[ord(TProp.State)] := ptrint(@obj.FPresentState); //TODO: why GetPropertyValue doesn't use get_State(x) in the original codebase?
    PropertyOffset2[ord(TProp.State)] := ptrint(StateEnum); 
    PropertyOffset3[ord(TProp.State)] := ptrint(@GetFuseStateSize);
    PropertyFlags[ord(TProp.State)] := [TPropertyFlag.SizeIsFunction]; // FControlledElement.NPhases

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TFuse.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TFuseObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    i: Integer;
begin
    case Idx of
        // Default the controlled element to the monitored element
        ord(TProp.MonitoredObj):
            ControlledElement := MonitoredElement;
        ord(TProp.MonitoredTerm):
            ElementTerminal := MonitoredElementTerminal;
        ord(TProp.Normal):
            NormalStateSet := TRUE;
        ord(TProp.State):
        begin
            if not NormalStateSet then 
            begin
                for i := 1 to FNPhases do 
                    FNormalState[i] := FPresentState[i];
                
                NormalStateSet := TRUE; // normal state will default to state only the 1st state is specified.
            end;
            if ControlledElement = NIL then
                Exit;

            ControlledElement.ActiveTerminalIdx := ElementTerminal;
            for i := 1 to ControlledElement.NPhases do
                if FPresentState[i] = CTRL_OPEN then
                    ControlledElement.Closed[i] := FALSE
                else
                    ControlledElement.Closed[i] := TRUE;
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TFuseObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    Other := TObj(OtherPtr);
    FNPhases := Other.Fnphases;
    NConds := Other.Fnconds; // Force Reallocation of terminal stuff

    ElementTerminal := Other.ElementTerminal;
    ControlledElement := Other.ControlledElement;  // Pointer to target circuit element

    MonitoredElement := Other.MonitoredElement;  // Pointer to target circuit element
    MonitoredElementTerminal := Other.MonitoredElementTerminal;  // Pointer to target circuit element

    FuseCurve := Other.FuseCurve;
    RatedCurrent := Other.RatedCurrent;

    for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do 
    begin
        FPresentState[i] := Other.FPresentState[i];
        FNormalState[i] := Other.FNormalState[i];
    end;
    CondOffset := Other.CondOffset;
end;

constructor TFuseObj.Create(ParClass: TDSSClass; const FuseName: String);
var
    i: Integer;
begin
    inherited Create(ParClass);
    Name := LowerCase(FuseName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 1;  // this forces allocation of terminals and conductors in base class
    ControlledElement := NIL;
    ElementTerminal := 1;

    MonitoredElementTerminal := 1;
    MonitoredElement := NIL;
    PreviousControlledElement := NIL;

    FuseCurve := TFuse(ParClass).TCC_CurveClass.Find('tlink');//TODO: is an error message ever required for this?

    RatedCurrent := 1.0;

    FPresentState := NIL;
    FNormalState := NIL;

    // Reallocate arrays (Must be initialized to nil for first call)
    Reallocmem(FPresentState, Sizeof(FPresentState[1]) * FNPhases);
    Reallocmem(FNormalState, Sizeof(FNormalState[1]) * FNPhases);

    for i := 1 to Min(FUSEMAXDIM, FNPhases) do 
    begin
        FPresentState^[i] := CTRL_CLOSE;
        FNormalState^[i] := CTRL_CLOSE; // default to present state
        ReadyToBlow[i] := FALSE;
        hAction[i] := 0;
    End;
    NormalStateSet := FALSE;

    cBuffer := NIL; // Complex buffer

    DSSObjType := ParClass.DSSClassType; //cap_CONTROL;

   //  RecalcElementData;
end;

destructor TFuseObj.Destroy;
begin
    Reallocmem(cBuffer, 0);
    ReallocMem(FPresentState, 0);
    ReallocMem(FNormalState, 0);

    inherited Destroy;
end;

procedure TFuseObj.RecalcElementData;
var
    i: Integer;
begin
    if MonitoredElement <> NIL then
    begin
        FNphases := MonitoredElement.NPhases; // Force number of phases to be same
        if Fnphases > FUSEMAXDIM then
            DoSimpleMsg('Warning: Fuse %s: Number of phases > Max fuse dimension.', [Self.Name], 404);
        if MonitoredElementTerminal > MonitoredElement.Nterms then
        begin
            DoErrorMsg(Format(_('Fuse: "%s"'), [Name]),
                Format(_('Terminal no. "%d" does not exist.'), [MonitoredElementTerminal]),
                _('Re-specify terminal no.'), 404);
        end
        else
        begin
            // Sets name of i-th terminal's connected bus in Fuse's buslist
            Setbus(1, MonitoredElement.GetBus(MonitoredElementTerminal));
            // Allocate a buffer big enough to hold everything from the monitored element
            ReAllocMem(cBuffer, SizeOF(cbuffer^[1]) * MonitoredElement.Yorder);
            CondOffset := (MonitoredElementTerminal - 1) * MonitoredElement.NConds; // for speedy sampling
        end;
    end;

    // Check for existence of Controlled Element

    // If previously assigned, reset HasOCPDevice flag in case this is a move
    if (PreviousControlledElement <> NIL) then
    begin
        Exclude(PreviousControlledElement.Flags, Flg.HasOCPDevice);
        PreviousControlledElement := ControlledElement;
    end;

    if ControlledElement <> NIL then
    begin  // Both CktElement and monitored element must already exist
        ControlledElement.ActiveTerminalIdx := ElementTerminal;  // Make the 1 st terminal active

        if Enabled then
            Include(ControlledElement.Flags, Flg.HasOCPDevice);  // For Reliability calcs

        // Open/Close State of controlled element based on state assigned to the control
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            if FPresentState[i] = CTRL_OPEN then
                ControlledElement.Closed[i] := FALSE
            else
                ControlledElement.Closed[i] := TRUE;

        for i := 1 to ControlledElement.Nphases do
            hAction[i] := 0;
        for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
            ReadyToBlow[i] := FALSE;
    end
    else
    begin
        // element not found
        DoErrorMsg(Format(_('Fuse: "%s"'), [Self.Name]),
            _('CktElement for SwitchedObj is not set.'),
            _('Element must be defined previously.'), 405);
    end;
end;

procedure TFuseObj.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;

procedure TFuseObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TFuseObj.DoPendingAction(const Phs, ProxyHdl: Integer);
// Do what we're instructed by the control queue
// Theoretically, there shouldn't be anything on the queue unless we have to do something

// Only legal action is to open one phase
begin
    if Phs > FUSEMAXDIM then
        Exit;

    with ControlledElement do
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal;
        if FPresentState[Phs] = CTRL_CLOSE then
            if ReadyToBlow[Phs] then
            begin   // ignore if we became disarmed in meantime
                ControlledElement.Closed[Phs] := FALSE;   // Open all phases of active terminal
                AppendtoEventLog('Fuse.' + Self.Name, 'Phase ' + IntToStr(Phs) + ' Blown');
                hAction[phs] := 0;
            end;
    end;
end;

procedure TFuseObj.Sample;
var
    i: Integer;
    Cmag: Double;
    TripTime: Double;
begin
    ControlledElement.ActiveTerminalIdx := ElementTerminal;
    MonitoredElement.GetCurrents(cBuffer);

    with MonitoredElement do
        for i := 1 to Min(FUSEMAXDIM, MonitoredElement.Nphases) do
        begin
            if ControlledElement.Closed[i]      // Check state of phases of active terminal
            then
                FPresentState[i] := CTRL_CLOSE
            else
                FPresentState[i] := CTRL_OPEN;

            if FPresentState[i] = CTRL_CLOSE then
            begin
                TripTime := -1.0;

                // Check Phase Trip, if any

                if FuseCurve <> NIL then
                begin
                    Cmag := Cabs(cBuffer^[i]);
                    TripTime := FuseCurve.GetTCCTime(Cmag / RatedCurrent);
                end;

                if TripTime > 0.0 then
                begin
                    if not ReadyToBlow[i] then
                        with ActiveCircuit do
                        begin  // Then arm for an open operation
                            hAction[i] := ControlQueue.Push(Solution.DynaVars.intHour, Solution.DynaVars.t + TripTime + Delaytime, i, 0, Self);
                            ReadyToBlow[i] := TRUE;
                        end;
                end
                else
                begin
                    if ReadyToBlow[i] then
                    begin  //  Current has dropped below pickup and it hasn't blown yet
                        ActiveCircuit.ControlQueue.Delete(hAction[i]);  // Delete the fuse blow action
                        ReadyToBlow[i] := FALSE;
                    end;
                end;
            end; // IF PresentState=CLOSE
        end;
end;

function TFuseObj.GetPropertyValue(Index: Integer): String;
begin
    if (Index = ord(TProp.Normal)) or (Index = ord(TProp.State)) then
        if ControlledElement = Nil Then
        begin
            Result := '[]'; // for compatibility with official OpenDSS
            Exit;
        end;

    Result := inherited GetPropertyValue(index);
end;

procedure TFuseObj.Reset;
var
    i: Integer;
begin
    if ControlledElement = NIL then
        Exit;

    ControlledElement.ActiveTerminalIdx := ElementTerminal;

    for i := 1 to Min(FUSEMAXDIM, ControlledElement.Nphases) do
    begin
        FPresentState[i] := FNormalState[i];  // reset to normal state
        ReadyToBlow[i] := FALSE;
        hAction[i] := 0;

        if FNormalState[i] = CTRL_OPEN then
            ControlledElement.Closed[i] := FALSE
        else
            ControlledElement.Closed[i] := TRUE;
    end;
end;

function TFuseObj.get_States(Idx: Integer): EControlAction;
begin
    if ControlledElement <> NIL then
    begin
        ControlledElement.ActiveTerminalIdx := ElementTerminal; 
        if not ControlledElement.Closed[Idx] then
            FPresentState[Idx]:= CTRL_OPEN
        else
            FPresentState[Idx]:= CTRL_CLOSE;
    end;
    Result := FPresentState[Idx];
end;

initialization
    PropInfo := NIL;
finalization
    ActionEnum.Free;
    StateEnum.Free;
end.
