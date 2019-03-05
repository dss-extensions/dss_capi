unit ImplCtrlQueue;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    Windows,
    ActiveX,
    Classes,
    ComObj,
    OpenDSSengine_TLB,
    StdVcl;

type
    TCtrlQueue = class(TAutoObject, ICtrlQueue)
    PRIVATE
    PROTECTED
        procedure ClearQueue; SAFECALL;
        procedure Delete(ActionHandle: Integer); SAFECALL;
        function Get_ActionCode: Integer; SAFECALL;
        function Get_DeviceHandle: Integer; SAFECALL;
        function Get_NumActions: Integer; SAFECALL;
        function Push(Hour: Integer; Seconds: Double; ActionCode,
            DeviceHandle: Integer): Integer; SAFECALL;
        procedure Show; SAFECALL;
        procedure ClearActions; SAFECALL;
        function Get_PopAction: Integer; SAFECALL;
        procedure Set_Action(Param1: Integer); SAFECALL;
        function Get_QueueSize: Integer; SAFECALL;
        procedure DoAllQueue; SAFECALL;
        function Get_Queue: Olevariant; SAFECALL;
    {Declare ICtrlQueue methods here}
    end;


implementation

uses
    ComServ,
    DSSGlobals,
    ControlQueue,
    ControlElem,
    DSSClass,
    Variants,
    sysutils,
    Utilities;

{Define class for proxy control object}

type
    pAction = ^Taction;

    TAction = record
        ActionCode: Integer;
        DeviceHandle: Integer;
    end;

    TCOMControlProxyObj = class(TControlElem)
    PRIVATE
        ActionList: TList;
        procedure ClearActionList;
        function PopAction: Boolean;
    PUBLIC

        constructor Create(ParClass: TDSSClass; const COMProxyName: String);
        destructor Destroy; OVERRIDE;

        procedure DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state
    end;

var
    COMControlProxyObj: TCOMControlProxyObj;
    ActiveAction: pAction;


procedure TCtrlQueue.Delete(ActionHandle: Integer);
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].ControlQueue.Delete(ActionHandle, ActiveActor);
    end;
end;


function TCtrlQueue.Get_ActionCode: Integer;
begin
    Result := 0;
    if ActiveAction <> NIL then
        Result := ActiveAction^.ActionCode;
end;

function TCtrlQueue.Get_DeviceHandle: Integer;
begin
    Result := 0;
    if ActiveAction <> NIL then
        Result := ActiveAction^.DeviceHandle;
end;

function TCtrlQueue.Get_NumActions: Integer;
begin
    Result := 0;
    Result := COMControlProxyObj.ActionList.Count;
end;


function TCtrlQueue.Push(Hour: Integer; Seconds: Double; ActionCode,
    DeviceHandle: Integer): Integer;

  // returns handle on control queue
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ControlQueue.push(Hour, Seconds, ActionCode, DeviceHandle, COMControlProxyObj, ActiveActor);
    end;
end;

procedure TCtrlQueue.Show;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
end;

{ TCOMControlProxyObj }

procedure TCOMControlProxyObj.ClearActionList;
begin
    while PopAction do ;  // spin until it is done
end;

constructor TCOMControlProxyObj.Create(ParClass: TDSSClass;
    const COMProxyName: String);
begin
    Name := COMProxyName;
    ActionList := TList.Create;
end;

destructor TCOMControlProxyObj.Destroy;
begin
    ClearActionList;
    ActionList.Free;
    inherited;
end;

procedure TCOMControlProxyObj.DoPendingAction(const Code, ProxyHdl: Integer; ActorID: Integer);
var
    Action: pAction;
begin
    Action := Allocmem(SizeOf(TAction));
    with Action^ do
    begin         // Capture the Action
        ActionCode := Code;
        DeviceHandle := ProxyHdl;
    end;
    ActionList.Add(Action);
end;

function TCOMControlProxyObj.PopAction: Boolean;
begin
    if ActiveAction <> NIL then
    begin
        Freemem(ActiveAction, Sizeof(TAction));
        ActiveAction := NIL;
    end;
    Result := TRUE;
    if ActionList.Count > 0 then
    begin
        ActiveAction := ActionList.Items[0];
        ActionList.Delete(0);
    end
    else
        Result := FALSE;
end;

procedure TCOMControlProxyObj.Reset;
begin
    ClearActionList;

end;


procedure TCtrlQueue.ClearActions;
begin
    COMControlProxyObj.ClearActionList;
end;

procedure TCtrlQueue.ClearQueue;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].ControlQueue.Clear;
    end;
end;


function TCtrlQueue.Get_PopAction: Integer;
begin
    Result := COMControlProxyObj.ActionList.Count;
    COMControlProxyObj.PopAction;
end;

procedure TCtrlQueue.Set_Action(Param1: Integer);
begin
    with COMControlProxyObj do
        if Param1 < ActionList.Count then
        begin
            ActiveAction := ActionList.Items[Param1 - 1];
        end;
end;

function TCtrlQueue.Get_QueueSize: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Result := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
    end;
end;

procedure TCtrlQueue.DoAllQueue;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        ActiveCircuit[ActiveActor].ControlQueue.DoAllActions(ActiveActor);
    end;
end;

function TCtrlQueue.Get_Queue: Olevariant;
// returns entire queue in CSV file format as a variant array of strings
var
    i: Integer;
    Qsize: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    QSize := Get_queuesize;
    if QSize > 0 then
    begin
        VarArrayRedim(Result, QSize);
        Result[0] := 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device';
        for i := 0 to QSize - 1 do
        begin
            Result[i + 1] := ActiveCircuit[ActiveActor].ControlQueue.QueueItem(i);
        end;
    end
    else
        Result[0] := 'No events';

end;

initialization
    TAutoObjectFactory.Create(ComServer, TCtrlQueue, Class_CtrlQueue,
        ciInternal, tmApartment);
 {Make a Proxy Control Object to receiving control actions}
    COMControlProxyObj := TCOMControlProxyObj.Create(NIL, 'COM_Proxy');
    ActiveAction := NIL;
end.
