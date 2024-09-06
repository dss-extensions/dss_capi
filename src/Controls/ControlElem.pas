unit ControlElem;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface


uses
    CktElement,
    Bus,
    UComplex, DSSUcomplex,
    DSSClass;

type
{$PUSH}
{$Z4} // keep enums as int32 values
    EControlAction = (
        CTRL_NONE,
        CTRL_OPEN,
        CTRL_CLOSE,
        CTRL_RESET,
        CTRL_LOCK,
        CTRL_UNLOCK,
        CTRL_TAPUP,
        CTRL_TAPDOWN
    );
{$POP}

    TControlElem = class(TDSSCktElement)

    PRIVATE
        procedure RemoveSelfFromControlElementList(cktElem: TDSSCktElement);
    PUBLIC
        controlledElement: TDSSCktElement;
        FMonitoredElement: TDSSCktElement;
        ElementTerminal: Integer;
        ControlledBusName: String;  // If different than terminal
        ControlledBus: TDSSBus;
        MonitorVariable: String;
        MonitorVarIndex: Integer;
        TimeDelay,
        DblTraceParameter: Double;
        ShowEventLog: LongBool;

        constructor Create(ParClass: TDSSClass; objName: String);
        destructor Destroy; OVERRIDE;

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Always Zero
        procedure CalcYPrim(); OVERRIDE; // Always Zero

        procedure Sample(); VIRTUAL;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); VIRTUAL;   // Do the action that is pending from last sample
        procedure Reset(); VIRTUAL;
        procedure SetControlledElement(const Value: TDSSCktElement);  // Pointer to target circuit element
        function MonitoredElement(): TDSSCktElement;
        procedure SetMonitoredElement(const Value: TDSSCktElement);
    end;

procedure SetMonitoredElement(obj: TControlElem; el: TDSSCktElement);
procedure SetControlledElement(obj: TControlElem; el: TDSSCktElement);

const
    USER_BASE_ACTION_CODE = 100;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    DSSPointerList;

procedure SetMonitoredElement(obj: TControlElem; el: TDSSCktElement);
begin
    obj.SetMonitoredElement(el);
end;

procedure SetControlledElement(obj: TControlElem; el: TDSSCktElement);
begin
    obj.SetControlledElement(el);
end;

constructor TControlElem.Create(ParClass: TDSSClass; objName: String);
begin
    inherited Create(ParClass, objName);
    DSSObjType := CTRL_ELEMENT;
    DblTraceParameter := 0.0;
    TimeDelay := 0.0;
    MonitorVariable := '';
    MonitorVarIndex := 0;
    controlledElement := NIL;
    ShowEventLog := DSS.EventLogDefault;
end;

destructor TControlElem.Destroy;
begin
    inherited Destroy;
end;

procedure TControlElem.DoPendingAction;
begin
  // virtual function - should be overridden
    DoSimpleMsg('Programming Error:  Reached base class for DoPendingAction.' + CRLF + 'Device: ' + FullName(), 460);
end;

procedure TControlElem.RemoveSelfFromControlElementList(cktElem: TDSSCktElement);
// Remove this control from the controlelementlist of the designated element
var
    ptr: TControlElem;
    TempList: TDSSPointerList;

begin
    // Make a new copy of the control element list
    TempList := TDSSPointerList.Create(1);
    for ptr in CktElem.ControlElementList do
    begin
        if ptr <> Self then
            TempList.Add(ptr);  // skip Self in copying list
    end;
    CktElem.ControlElementList.Free;
    CktElem.ControlElementList := TempList;
end;

procedure TControlElem.Reset();
begin
    DoSimpleMsg('Programming Error: Reached base class for Reset.' + CRLF + 'Device: ' + FullName(), 461);
end;

procedure TControlElem.Sample();
begin
    // virtual function - should be overridden
    DoSimpleMsg('Programming Error:  Reached base class for Sample.' + CRLF + 'Device: ' + FullName(), 462);
end;

procedure TControlElem.SetControlledElement(const Value: TDSSCktElement);
begin
    try
      // Check for reassignment of Controlled element and remove from list
        if controlledElement <> NIL then
        begin
            if controlledElement.ControlElementList.Count = 1 then
                Exclude(controlledElement.Flags, Flg.HasControl);
            RemoveSelfFromControlElementList(controlledElement);
        end;
    finally
        controlledElement := Value;
        if controlledElement <> NIL then
        begin
            Include(controlledElement.Flags, Flg.HasControl);
            controlledElement.ControlElementList.Add(Self);
        end;
    end;
end;

function TControlElem.MonitoredElement(): TDSSCktElement;
begin
    result := FMonitoredElement;
end;

procedure TControlElem.SetMonitoredElement(const Value: TDSSCktElement);
begin
    FMonitoredElement := Value;
    if FMonitoredElement <> NIL then
        Include(FMonitoredElement.Flags, Flg.IsMonitored);
end;

procedure TControlElem.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to FNConds do
        Curr[i] := 0;
end;

procedure TControlElem.CalcYPrim();
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;


end.
