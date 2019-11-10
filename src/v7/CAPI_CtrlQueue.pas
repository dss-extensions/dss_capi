unit CAPI_CtrlQueue;

{$inline on}

interface

uses
    CAPI_Utils,
    Classes;

procedure CtrlQueue_ClearQueue(); CDECL;
procedure CtrlQueue_Delete(ActionHandle: Integer); CDECL;
function CtrlQueue_Get_ActionCode(): Integer; CDECL;
function CtrlQueue_Get_DeviceHandle(): Integer; CDECL;
function CtrlQueue_Get_NumActions(): Integer; CDECL;
function CtrlQueue_Push(Hour: Integer; Seconds: Double; ActionCode, DeviceHandle: Integer): Integer; CDECL;
procedure CtrlQueue_Show(); CDECL;
procedure CtrlQueue_ClearActions(); CDECL;
function CtrlQueue_Get_PopAction(): Integer; CDECL;
procedure CtrlQueue_Set_Action(Param1: Integer); CDECL;
function CtrlQueue_Get_QueueSize(): Integer; CDECL;
procedure CtrlQueue_DoAllQueue(); CDECL;
procedure CtrlQueue_Get_Queue(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure CtrlQueue_Get_Queue_GR(); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    ControlQueue,
    ControlElem,
    DSSClass,
    sysutils,
    Utilities,
    DSSHelper;


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

        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state
    end;

var
    COMControlProxyObj: TCOMControlProxyObj;
    ActiveAction: pAction;

procedure CtrlQueue_ClearQueue(); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.ControlQueue.Clear;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Delete(ActionHandle: Integer); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.ControlQueue.Delete(ActionHandle);
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_ActionCode(): Integer; CDECL;
begin
    Result := 0;
    if ActiveAction <> NIL then
        Result := ActiveAction^.ActionCode;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_DeviceHandle(): Integer; CDECL;
begin
    Result := 0;
    if ActiveAction <> NIL then
        Result := ActiveAction^.DeviceHandle;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_NumActions(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then Exit;
    Result := COMControlProxyObj.ActionList.Count;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Push(Hour: Integer; Seconds: Double; ActionCode, DeviceHandle: Integer): Integer; CDECL;
// returns handle on control queue
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        Result := DSSPrime.ActiveCircuit.ControlQueue.push(Hour, Seconds, ActionCode, DeviceHandle, COMControlProxyObj);
    end;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Show(); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.ControlQueue.ShowQueue(DSSPrime.OutputDirectory + 'COMProxy_ControlQueue.CSV');
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_ClearActions(); CDECL;
begin
    if DSSPrime.ActiveCircuit = NIL then Exit;
    COMControlProxyObj.ClearActionList;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_PopAction(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then Exit;
    Result := COMControlProxyObj.ActionList.Count;
    COMControlProxyObj.PopAction;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Set_Action(Param1: Integer); CDECL;
begin
    if DSSPrime.ActiveCircuit = NIL then Exit;
    with COMControlProxyObj do
        if Param1 < ActionList.Count then
            ActiveAction := ActionList.Items[Param1 - 1];
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_QueueSize(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.ControlQueue.QueueSize;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_DoAllQueue(); CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        DSSPrime.ActiveCircuit.ControlQueue.DoAllActions;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Get_Queue(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
// returns entire queue in CSV file format as a variant array of strings
var
    Result: PPAnsiCharArray;
    i: Integer;
    Qsize: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        QSize := CtrlQueue_Get_queuesize;
        if QSize > 0 then
        begin
            DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (QSize) + 1);
            Result[0] := DSS_CopyStringAsPChar('Handle, Hour, Sec, ActionCode, ProxyDevRef, Device');
            for i := 0 to QSize - 1 do
            begin
                Result[i + 1] := DSS_CopyStringAsPChar(DSSPrime.ActiveCircuit.ControlQueue.QueueItem(i));
            end;
            Exit;
        end;
    end;
    Result[0] := DSS_CopyStringAsPChar('No events');
end;

procedure CtrlQueue_Get_Queue_GR(); CDECL;
// Same as CtrlQueue_Get_Queue but uses global result (GR) pointers
begin
    CtrlQueue_Get_Queue(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
{ TCOMControlProxyObj }
procedure TCOMControlProxyObj.DoPendingAction(const Code, ProxyHdl: Integer);
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

initialization
 {Make a Proxy Control Object to receiving control actions}
    COMControlProxyObj := TCOMControlProxyObj.Create(NIL, 'COM_Proxy');
    ActiveAction := NIL;

end.
