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
    ucomplex,
    DSSClass;

{$INCLUDE ControlActionDefs.txt}

type

    TControlElem = class(TDSSCktElement)

    PRIVATE
        FControlledElement: TDSSCktElement;
        procedure Set_ControlledElement(const Value: TDSSCktElement);  // Pointer to target circuit element
        procedure RemoveSelfFromControlelementList(CktElem: TDSSCktElement);
    PUBLIC

        ElementName: String;
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

        procedure Sample(ActorID: Integer); VIRTUAL;    // Sample control quantities and set action times in Control Queue
        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); VIRTUAL;   // Do the action that is pending from last sample
        procedure Reset; VIRTUAL;

        property ControlledElement: TDSSCktElement READ FControlledElement WRITE Set_ControlledElement;

    end;

const
    USER_BASE_ACTION_CODE = 100;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    PointerList;

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

procedure TControlElem.DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer);
begin
  // virtual function - should be overridden
    DoSimpleMsg('Programming Error:  Reached base class for DoPendingAction.' + CRLF + 'Device: ' + DSSClassName + '.' + Name, 460);
end;

procedure TControlElem.RemoveSelfFromControlElementList(CktElem: TDSSCktElement);
{Remove this control from the controlelementlist of the designated element}
var
    ptr: TControlElem;
    TempList: TPointerList;
    i: Integer;

begin
    with CktElem do
    begin
         // Make a new copy of the control element list
        TempList := TPointerList.Create(1);
        for i := 1 to ControlElementList.ListSize do
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
    DoSimpleMsg('Programming Error: Reached base class for Reset.' + CRLF + 'Device: ' + DSSClassName + '.' + Name, 461);
end;

procedure TControlElem.Sample(ActorID: Integer);
begin
  // virtual function - should be overridden
    DoSimpleMsg('Programming Error:  Reached base class for Sample.' + CRLF + 'Device: ' + DSSClassName + '.' + Name, 462);
end;


procedure TControlElem.Set_ControlledElement(const Value: TDSSCktElement);
begin

    try
      // Check for reassignment of Controlled element and remove from list
        if Assigned(FControlledElement) then
            with FControlledElement do
            begin
                if ControlElementList.ListSize = 1 then
                    HasControl := FALSE;
                RemoveSelfFromControlElementList(FControlledElement);
            end;
    finally
        FControlledElement := Value;
        if Assigned(FControlledElement) then
            with FControlledElement do
            begin
                HasControl := TRUE;
                ControlElementList.Add(Self);
            end;
    end;
end;

end.
