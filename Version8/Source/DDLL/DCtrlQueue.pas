unit DCtrlQueue;

interface

uses
    Windows,
    ActiveX,
    Classes,
    ComObj;

function CtrlQueueI(mode: Longint; arg: Longint): Longint; CDECL;
procedure CtrlQueueV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    ComServ,
    DSSGlobals,
    ControlQueue,
    ControlElem,
    DSSClass,
    Variants;

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
    ActiveAction: pAction;

procedure TCOMControlProxyObj.ClearActionList;
begin
    while PopAction do ;  // spin until it is done
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

procedure TCOMControlProxyObj.Reset;
begin
    ClearActionList;

end;

function CtrlQueueI(mode: Longint; arg: Longint): Longint; CDECL;

var
    COMControlProxyObj: TCOMControlProxyObj;
    Hour: Integer;
    Seconds: Double;
    ActionCode, DeviceHandle: Integer;

begin
    Result := 0;
    case mode of
        0:
        begin  // CtrlQueue.ClearQueue
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].ControlQueue.Clear;
            end;
        end;
        1:
        begin // CtrlQueue.Delete
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].ControlQueue.Delete(arg, ActiveActor);
            end;
        end;
        2:
        begin  // CtrlQueue.NumActions
            Result := 0;
            Result := COMControlProxyObj.ActionList.Count;
        end;
        3:
        begin  // CtrlQueue.Action
            with COMControlProxyObj do
                if arg < ActionList.Count then
                begin
                    ActiveAction := ActionList.Items[arg - 1];
                end;
        end;
        4:
        begin  // CtrlQueue.ActionCode
            Result := 0;
            if ActiveAction <> NIL then
                Result := ActiveAction^.ActionCode;
        end;
        5:
        begin  // CtrlQueue.DeviceHandle
            Result := 0;
            if ActiveAction <> NIL then
                Result := ActiveAction^.DeviceHandle;
        end;
        6:
        begin  // CtrlQueue.Push
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].ControlQueue.push(Hour, Seconds, ActionCode, DeviceHandle, COMControlProxyObj, ActiveActor);
            end;
        end;
        7:
        begin  // CtrlQueue.Show
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
        end;
        8:
        begin  // CtrlQueue.ClearActions
            COMControlProxyObj.ClearActionList;
        end;
        9:
        begin  // CtrlQueue.PopAction
            Result := COMControlProxyObj.ActionList.Count;
            COMControlProxyObj.PopAction;
        end;
        10:
        begin // CtrlQueue.Get_QueueSize
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
            end;
        end;
        11:
        begin // CtrlQueue.DoAllQueue
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                ActiveCircuit[ActiveActor].ControlQueue.DoAllActions(ActiveActor);
            end;
        end
    else
        Result := -1;
    end;
end;

procedure CtrlQueueV(mode: Longint; out arg: Variant); CDECL;
var
    i: Integer;
    Qsize: Integer;
begin
    case mode of
        0:
        begin  // CtrlQueue.ClearQueue
            arg := VarArrayCreate([0, 0], varOleStr);
            QSize := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
            if QSize > 0 then
            begin
                VarArrayRedim(arg, QSize);
                arg[0] := 'Handle, Hour, Sec, ActionCode, ProxyDevRef, Device';
                for i := 0 to QSize - 1 do
                begin
                    arg[i + 1] := ActiveCircuit[ActiveActor].ControlQueue.QueueItem(i);
                end;
            end
            else
                arg[0] := 'No events';
        end
    else
        arg := VarArrayCreate([0, 0], varOleStr);
        arg[0] := 'Mode not recognized';
    end;
end;

end.
