unit DCtrlQueue;

interface

uses
  {$IFNDEF FPC_DLL}
    Windows,
    ActiveX,
    ComObj,
{$ENDIF}
    Classes;

function CtrlQueueI(mode: Longint; arg: Longint): Longint; CDECL;
procedure CtrlQueueV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
{$ENDIF}
    DSSGlobals,
    ControlQueue,
    ControlElem,
    DSSClass,
    Variants,
    SysUtils,
    ExceptionTrace;

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
        procedure Reset(ActorID: Integer); OVERRIDE;  // Reset to initial defined state
    end;


var
    ActiveAction: pAction;
    COMControlProxyObj: TCOMControlProxyObj;

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
            if ActiveAction <> NIL then
                Result := ActiveAction^.ActionCode;
        end;
        5:
        begin  // CtrlQueue.DeviceHandle
            if ActiveAction <> NIL then
                Result := ActiveAction^.DeviceHandle;
        end;
        6:
        begin  // CtrlQueue.Show
            if ActiveCircuit[ActiveActor] <> NIL then
                ActiveCircuit[ActiveActor].ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
        end;
        7:
        begin  // CtrlQueue.ClearActions
            COMControlProxyObj.ClearActionList;
        end;
        8:
        begin  // CtrlQueue.PopAction
            Result := COMControlProxyObj.ActionList.Count;
            COMControlProxyObj.PopAction;
        end;
        9:
        begin // CtrlQueue.Get_QueueSize
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                Result := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
            end;
        end;
        10:
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

procedure CtrlQueueV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    Hour,
    ActionCode,
    DeviceHandle,
    Seconds: Double;
    pDbl: ^Double;
    i,
    Qsize: Integer;
begin
    case mode of
        0:
        begin  // CtrlQueue.ClearQueue
            myType := 4;        // String
            setlength(myStrArray, 0);
            QSize := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
            if QSize > 0 then
            begin
                WriteStr2Array('Handle, Hour, Sec, ActionCode, ProxyDevRef, Device');
                WriteStr2Array(Char(0));
                for i := 0 to QSize - 1 do
                begin
                    WriteStr2Array(ActiveCircuit[ActiveActor].ControlQueue.QueueItem(i));
                    WriteStr2Array(Char(0));
                end;
            end
            else
                WriteStr2Array('No events');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // CtrlQueue.Push
            myType := 2;        // Double
            pDbl := myPointer;
            QSize := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                try
                    begin
                        Hour := pDBL^;
                        inc(Pbyte(pDBL), 8);
                        Seconds := pDBL^;
                        inc(Pbyte(pDBL), 8);
                        ActionCode := pDBL^;
                        inc(Pbyte(pDBL), 8);
                        DeviceHandle := pDBL^;
                        QSize := ActiveCircuit[ActiveActor].ControlQueue.push(trunc(Hour), Seconds, trunc(ActionCode), trunc(DeviceHandle), COMControlProxyObj, ActiveActor);
                    end;
                except
                    Qsize := -10001;              // Something went wrong;
                end;
            end;
            mySize := QSize;
        end
    else
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
end;

initialization
  {$IFDEF FPC_TRACE_INIT}
    writeln(format('init %s:%s', [{$I %FILE%}, {$I %LINE%}]));
{$ENDIF}
 {Make a Proxy Control Object to receiving control actions}
    try
        COMControlProxyObj := TCOMControlProxyObj.Create(NIL, 'COM_Proxy');
        ActiveAction := NIL;
    except
        On E: Exception do
            DumpExceptionCallStack(E);
    end;
end.
