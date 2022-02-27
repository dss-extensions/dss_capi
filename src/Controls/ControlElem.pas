unit ControlElem;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
        procedure RemoveSelfFromControlelementList(CktElem: TDSSCktElement);
    PUBLIC
        FControlledElement: TDSSCktElement;
        FMonitoredElement: TDSSCktElement;
        ElementTerminal: Integer;
        ControlledBusName: String;  // If different than terminal
        ControlledBus: TDSSBus;
        MonitorVariable: String;
        MonitorVarIndex: Integer;
        TimeDelay,
        DblTraceParameter: Double;
        ShowEventLog: Boolean;

        constructor Create(ParClass: TDSSClass);
        destructor Destroy; OVERRIDE;

        procedure GetCurrents(Curr: pComplexArray); OVERRIDE; // Always Zero
        procedure CalcYPrim; OVERRIDE; // Always Zero

        procedure Sample; VIRTUAL;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer); VIRTUAL;   // Do the action that is pending from last sample
        procedure Reset; VIRTUAL;
        procedure Set_ControlledElement(const Value: TDSSCktElement);  // Pointer to target circuit element
        procedure Set_MonitoredElement(const Value: TDSSCktElement);
        property ControlledElement: TDSSCktElement READ FControlledElement WRITE Set_ControlledElement;
        property MonitoredElement: TDSSCktElement READ FMonitoredElement WRITE Set_MonitoredElement;
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
    obj.Set_MonitoredElement(el);
end;

procedure SetControlledElement(obj: TControlElem; el: TDSSCktElement);
begin
    obj.Set_ControlledElement(el);
end;

constructor TControlElem.Create(ParClass: TDSSClass);
begin
    inherited Create(ParClass);
    DSSObjType := CTRL_ELEMENT;
    DblTraceParameter := 0.0;
    TimeDelay := 0.0;
    MonitorVariable := '';
    MonitorVarIndex := 0;
    FControlledElement := NIL;
    ShowEventLog := TRUE;
end;

destructor TControlElem.Destroy;
begin
    inherited Destroy;
end;

procedure TControlElem.DoPendingAction;
begin
  // virtual function - should be overridden
    DoSimpleMsg('Programming Error:  Reached base class for DoPendingAction.' + CRLF + 'Device: ' + FullName, 460);
end;

procedure TControlElem.RemoveSelfFromControlElementList(CktElem: TDSSCktElement);
{Remove this control from the controlelementlist of the designated element}
var
    ptr: TControlElem;
    TempList: TDSSPointerList;
    i: Integer;

begin
    with CktElem do
    begin
         // Make a new copy of the control element list
        TempList := TDSSPointerList.Create(1);
        for i := 1 to ControlElementList.Count do
        begin
            ptr := ControlElementList.Get(i);
            if ptr <> Self then
                TempList.Add(ptr);  // skip Self in copying list
        end;
        ControlElementList.Free;
        ControlElementList := TempList;
    end;
end;

procedure TControlElem.Reset;
begin
    DoSimpleMsg('Programming Error: Reached base class for Reset.' + CRLF + 'Device: ' + FullName, 461);
end;

procedure TControlElem.Sample;
begin
  // virtual function - should be overridden
    DoSimpleMsg('Programming Error:  Reached base class for Sample.' + CRLF + 'Device: ' + FullName, 462);
end;

procedure TControlElem.Set_ControlledElement(const Value: TDSSCktElement);
begin
    try
      // Check for reassignment of Controlled element and remove from list
        if Assigned(FControlledElement) then
            with FControlledElement do
            begin
                if ControlElementList.Count = 1 then
                    Exclude(Flags, Flg.HasControl);
                RemoveSelfFromControlElementList(FControlledElement);
            end;
    finally
        FControlledElement := Value;
        if Assigned(FControlledElement) then
            with FControlledElement do
            begin
                Include(Flags, Flg.HasControl);
                ControlElementList.Add(Self);
            end;
    end;
end;

procedure TControlElem.Set_MonitoredElement(const Value: TDSSCktElement);
begin
    FMonitoredElement := Value;
    if Assigned(FMonitoredElement) then
        Include(FMonitoredElement.Flags, Flg.IsMonitored);
end;

procedure TControlElem.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    for i := 1 to Fnconds do
        Curr^[i] := CZERO;
end;

procedure TControlElem.CalcYPrim;
begin
  // leave YPrims as nil and they will be ignored
  // Yprim is zeroed when created.  Leave it as is.
  //  IF YPrim=nil THEN YPrim := TcMatrix.CreateMatrix(Yorder);
end;


end.
