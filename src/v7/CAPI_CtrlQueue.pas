UNIT CAPI_CtrlQueue;
{$inline on}

INTERFACE

USES CAPI_Utils, Classes;

procedure CtrlQueue_ClearQueue();cdecl;
procedure CtrlQueue_Delete(ActionHandle: Integer);cdecl;
function CtrlQueue_Get_ActionCode():Integer;cdecl;
function CtrlQueue_Get_DeviceHandle():Integer;cdecl;
function CtrlQueue_Get_NumActions():Integer;cdecl;
procedure CtrlQueue_Show();cdecl;
procedure CtrlQueue_ClearActions();cdecl;
function CtrlQueue_Get_PopAction():Integer;cdecl;
procedure CtrlQueue_Set_Action(Param1: Integer);cdecl;
function CtrlQueue_Get_QueueSize():Integer;cdecl;
procedure CtrlQueue_DoAllQueue();cdecl;
PROCEDURE CtrlQueue_Get_Queue(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, ControlQueue, ControlElem, DSSClass, sysutils, Utilities;


{Define class for proxy control object}

Type
  pAction = ^Taction;
  TAction = Record
       ActionCode :Integer;
       DeviceHandle :Integer;
  End;

  TCOMControlProxyObj = class(TControlElem)
     private
       ActionList :TList;
       Procedure ClearActionList;
       Function PopAction: Boolean;
     public

       constructor Create(ParClass:TDSSClass; const COMProxyName:String);
       destructor Destroy; override;

       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state
  end;

Var
    COMControlProxyObj :TCOMControlProxyObj;
    ActiveAction       :pAction;
   
procedure CtrlQueue_ClearQueue();cdecl;
begin
   If ActiveCircuit <> Nil then Begin
      ActiveCircuit.ControlQueue.Clear;
   End;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Delete(ActionHandle: Integer);cdecl;
begin
    If ActiveCircuit <> Nil then Begin
      ActiveCircuit.ControlQueue.Delete(ActionHandle);
   End;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_ActionCode():Integer;cdecl;
begin
   Result := 0;
    If ActiveAction<> NIl then   Result := ActiveAction^.ActionCode ;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_DeviceHandle():Integer;cdecl;
begin
   Result := 0;
    If ActiveAction<> NIl then   Result := ActiveAction^.DeviceHandle;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_NumActions():Integer;cdecl;
begin
   Result := 0;
     Result := COMControlProxyObj.ActionList.Count;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Show();cdecl;
begin
     If ActiveCircuit <> Nil then
        ActiveCircuit.ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
end;

{ TCOMControlProxyObj }
//------------------------------------------------------------------------------
procedure CtrlQueue_ClearActions();cdecl;
begin
      COMControlProxyObj.ClearActionList;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_PopAction():Integer;cdecl;
begin
     Result := COMControlProxyObj.ActionList.Count;
     COMControlProxyObj.PopAction;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_Set_Action(Param1: Integer);cdecl;
begin
    With COMControlProxyObj Do
     If Param1 < ActionList.Count then Begin
       ActiveAction := ActionList.Items[Param1 - 1];
     End;
end;
//------------------------------------------------------------------------------
function CtrlQueue_Get_QueueSize():Integer;cdecl;
begin
   If ActiveCircuit <> Nil then Begin
      Result := ActiveCircuit.ControlQueue.QueueSize;
   End;
end;
//------------------------------------------------------------------------------
procedure CtrlQueue_DoAllQueue();cdecl;
begin
    If ActiveCircuit <> Nil then Begin
      ActiveCircuit.ControlQueue.DoAllActions;
   End;
end;
//------------------------------------------------------------------------------
PROCEDURE CtrlQueue_Get_Queue(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
// returns entire queue in CSV file format as a variant array of strings
VAR
  Result: PPAnsiCharArray;
  i     : integer;
  Qsize : integer;

begin
      Result  := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
      QSize   := CtrlQueue_Get_queuesize;
      if QSize > 0 then
      begin
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (QSize) + 1);
        Result[0]:=DSS_CopyStringAsPChar('Handle, Hour, Sec, ActionCode, ProxyDevRef, Device');
        For i := 0 to QSize-1 do
          Begin
            Result[i+1]:= DSS_CopyStringAsPChar(ActiveCircuit.ControlQueue.QueueItem(i));
          End;
      end
      else Result[0]:=DSS_CopyStringAsPChar('No events');

end;
//------------------------------------------------------------------------------
procedure TCOMControlProxyObj.DoPendingAction(const Code, ProxyHdl: Integer);
Var
   Action :pAction;
begin
     Action := Allocmem(SizeOf(TAction));
     With Action^ Do Begin         // Capture the Action
          ActionCode := Code;
          DeviceHandle := ProxyHdl;
     End;
     ActionList.Add(Action);
end;                
                
{ TCOMControlProxyObj }

procedure TCOMControlProxyObj.ClearActionList;
begin
   while PopAction do   ;  // spin until it is done
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
    If ActiveAction <> Nil then  Begin
      Freemem(ActiveAction, Sizeof(TAction));
      ActiveAction := Nil;
    End;
    Result := TRUE;
    If ActionList.Count>0 then Begin
       ActiveAction := ActionList.Items[0];
       ActionList.Delete(0);
    End Else Result := FALSE;
end;

procedure TCOMControlProxyObj.Reset;
begin
  ClearActionList;

end;
            
initialization            
 {Make a Proxy Control Object to receiving control actions}
    COMControlProxyObj := TCOMControlProxyObj.Create(Nil, 'COM_Proxy');
    ActiveAction := Nil;
            
END.
