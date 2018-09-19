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
  Windows, ActiveX, Classes, ComObj, OpenDSSengine_TLB, StdVcl;

type
  TCtrlQueue = class(TAutoObject, ICtrlQueue)
  private
  protected
    procedure ClearQueue; safecall;
    procedure Delete(ActionHandle: Integer); safecall;
    function Get_ActionCode: Integer; safecall;
    function Get_DeviceHandle: Integer; safecall;
    function Get_NumActions: Integer; safecall;
    function Push(Hour: Integer; Seconds: Double; ActionCode,
      DeviceHandle: Integer): Integer; safecall;
    procedure Show; safecall;
    procedure ClearActions; safecall;
    function Get_PopAction: Integer; safecall;
    procedure Set_Action(Param1: Integer); safecall;
    function Get_QueueSize: Integer; safecall;
    procedure DoAllQueue; safecall;
    function Get_Queue: OleVariant; safecall;
    {Declare ICtrlQueue methods here}
  end;




implementation

uses  ComServ, DSSGlobals, ControlQueue, ControlElem, DSSClass, Variants,
      sysutils, Utilities;

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

       PROCEDURE DoPendingAction(Const Code, ProxyHdl:Integer; ActorID : Integer); Override;   // Do the action that is pending from last sample
       PROCEDURE Reset; Override;  // Reset to initial defined state
  end;

Var
    COMControlProxyObj :TCOMControlProxyObj;
    ActiveAction       :pAction;



procedure TCtrlQueue.Delete(ActionHandle: Integer);
begin
    If ActiveCircuit[ActiveActor] <> Nil then Begin
      ActiveCircuit[ActiveActor].ControlQueue.Delete(ActionHandle, ActiveActor);
   End;
end;


function TCtrlQueue.Get_ActionCode: Integer;
begin
   Result := 0;
    If ActiveAction<> NIl then   Result := ActiveAction^.ActionCode ;
end;

function TCtrlQueue.Get_DeviceHandle: Integer;
begin
   Result := 0;
    If ActiveAction<> NIl then   Result := ActiveAction^.DeviceHandle;
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
   If ActiveCircuit[ActiveActor] <> Nil then Begin
      Result := ActiveCircuit[ActiveActor].ControlQueue.push(Hour, Seconds, ActionCode, DeviceHandle, COMControlProxyObj, ActiveActor);
   End;
end;

procedure TCtrlQueue.Show;
begin
     If ActiveCircuit[ActiveActor] <> Nil then
        ActiveCircuit[ActiveActor].ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
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

procedure TCOMControlProxyObj.DoPendingAction(const Code, ProxyHdl: Integer; ActorID : Integer);
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


procedure TCtrlQueue.ClearActions;
begin
      COMControlProxyObj.ClearActionList;
end;

procedure TCtrlQueue.ClearQueue;
begin
   If ActiveCircuit[ActiveActor] <> Nil then Begin
      ActiveCircuit[ActiveActor].ControlQueue.Clear;
   End;
end;


function TCtrlQueue.Get_PopAction: Integer;
begin
     Result := COMControlProxyObj.ActionList.Count;
     COMControlProxyObj.PopAction;
end;

procedure TCtrlQueue.Set_Action(Param1: Integer);
begin
    With COMControlProxyObj Do
     If Param1 < ActionList.Count then Begin
       ActiveAction := ActionList.Items[Param1 - 1];
     End;
end;

function TCtrlQueue.Get_QueueSize: Integer;
begin
   If ActiveCircuit[ActiveActor] <> Nil then Begin
      Result := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
   End;
end;

procedure TCtrlQueue.DoAllQueue;
begin
    If ActiveCircuit[ActiveActor] <> Nil then Begin
      ActiveCircuit[ActiveActor].ControlQueue.DoAllActions(ActiveActor);
   End;
end;

function TCtrlQueue.Get_Queue: OleVariant;
// returns entire queue in CSV file format as a variant array of strings
Var
  i     : integer;
  Qsize : integer;

begin
      Result  := VarArrayCreate([0, 0], varOleStr);
      QSize   := Get_queuesize;
      if QSize > 0 then
      begin
        VarArrayRedim(Result, QSize);
        Result[0]:='Handle, Hour, Sec, ActionCode, ProxyDevRef, Device';
        For i := 0 to QSize-1 do
          Begin
            Result[i+1]:= ActiveCircuit[ActiveActor].ControlQueue.QueueItem(i);
          End;
      end
      else Result[0]:='No events';

end;

initialization
  TAutoObjectFactory.Create(ComServer, TCtrlQueue, Class_CtrlQueue,
    ciInternal, tmApartment);
 {Make a Proxy Control Object to receiving control actions}
    COMControlProxyObj := TCOMControlProxyObj.Create(Nil, 'COM_Proxy');
    ActiveAction := Nil;
end.
