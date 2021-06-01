unit CAPI_CtrlQueue;

interface

uses
    CAPI_Utils,
    CAPI_Types,
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
procedure CtrlQueue_Get_Queue(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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

procedure CtrlQueue_ClearQueue(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ControlQueue.Clear();
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Delete(ActionHandle: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ControlQueue.Delete(ActionHandle);
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_ActionCode(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveAction^.ActionCode;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_DeviceHandle(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveAction^.DeviceHandle;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_NumActions(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ControlProxyObj.ActionList.Count;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Push(Hour: Integer; Seconds: Double; ActionCode, DeviceHandle: Integer): Integer; CDECL;
// returns handle on control queue
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ControlQueue.push(Hour, Seconds, ActionCode, DeviceHandle, DSSPrime.ControlProxyObj);
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Show(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ControlQueue.ShowQueue(DSSPrime.OutputDirectory + 'COMProxy_ControlQueue.CSV');
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_ClearActions(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ControlProxyObj.ClearActionList;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_PopAction(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ControlProxyObj.ActionList.Count;
    DSSPrime.ControlProxyObj.PopAction;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Set_Action(Param1: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    with DSSPrime.ControlProxyObj do
        if Param1 < ActionList.Count then
            DSSPrime.ActiveAction := ActionList.Items[Param1 - 1];
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_QueueSize(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ControlQueue.QueueSize;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_DoAllQueue(); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ControlQueue.DoAllActions;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Get_Queue(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
// returns entire queue in CSV file format as a variant array of strings
var
    Result: PPAnsiCharArray0;
    i: Integer;
    Qsize: Integer;

begin
    if not InvalidCircuit(DSSPrime) then
    begin
        QSize := CtrlQueue_Get_QueueSize();
        if QSize > 0 then
        begin
            DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, QSize + 1);
            Result[0] := DSS_CopyStringAsPChar('Handle, Hour, Sec, ActionCode, ProxyDevRef, Device');
            for i := 0 to QSize - 1 do
            begin
                Result[i + 1] := DSS_CopyStringAsPChar(DSSPrime.ActiveCircuit.ControlQueue.QueueItem(i));
            end;
            Exit;
        end;
    end;
    DefaultResult(ResultPtr, ResultCount, 'No events');
end;

procedure CtrlQueue_Get_Queue_GR(); CDECL;
// Same as CtrlQueue_Get_Queue but uses global result (GR) pointers
begin
    CtrlQueue_Get_Queue(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

end.
