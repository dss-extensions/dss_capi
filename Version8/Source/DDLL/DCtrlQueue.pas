unit DCtrlQueue;

interface

uses
  {$IFNDEF FPC_DLL}Windows, ActiveX, ComObj, {$ENDIF}Classes;

function CtrlQueueI(mode: longint; arg: longint):longint;cdecl;
procedure CtrlQueueV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;

implementation

uses {$IFNDEF FPC_DLL}ComServ, {$ENDIF}DSSGlobals, ControlQueue, ControlElem, DSSClass, Variants, SysUtils, ExceptionTrace;

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
       PROCEDURE Reset(ActorID: Integer); Override;  // Reset to initial defined state
  end;


var
    ActiveAction       :pAction;
    COMControlProxyObj :TCOMControlProxyObj;

procedure TCOMControlProxyObj.ClearActionList;
begin
   while PopAction do   ;  // spin until it is done
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

procedure TCOMControlProxyObj.Reset;
begin
  ClearActionList;

end;

function CtrlQueueI(mode: longint; arg: longint):longint;cdecl;
begin
  Result := 0;
  case mode of
  0: begin  // CtrlQueue.ClearQueue
     If ActiveCircuit[ActiveActor] <> Nil then Begin
        ActiveCircuit[ActiveActor].ControlQueue.Clear;
     End;
  end;
  1: begin // CtrlQueue.Delete
      If ActiveCircuit[ActiveActor] <> Nil then Begin
        ActiveCircuit[ActiveActor].ControlQueue.Delete(arg,ActiveActor);
     End;
  end;
  2: begin  // CtrlQueue.NumActions
      Result := COMControlProxyObj.ActionList.Count;
  end;
  3: begin  // CtrlQueue.Action
      With COMControlProxyObj Do
       If arg < ActionList.Count then Begin
         ActiveAction := ActionList.Items[arg - 1];
      End;
  end;
  4: begin  // CtrlQueue.ActionCode
       If ActiveAction<> NIl then   Result := ActiveAction^.ActionCode;
  end;
  5: begin  // CtrlQueue.DeviceHandle
       If ActiveAction<> NIl then   Result := ActiveAction^.DeviceHandle;
  end;
  6: begin  // CtrlQueue.Show
       If ActiveCircuit[ActiveActor] <> Nil then
          ActiveCircuit[ActiveActor].ControlQueue.ShowQueue(DSSDirectory + 'COMProxy_ControlQueue.CSV');
  end;
  7: begin  // CtrlQueue.ClearActions
      COMControlProxyObj.ClearActionList;
  end;
  8: begin  // CtrlQueue.PopAction
     Result := COMControlProxyObj.ActionList.Count;
     COMControlProxyObj.PopAction;
  end;
  9: begin // CtrlQueue.Get_QueueSize
     If ActiveCircuit[ActiveActor] <> Nil then Begin
        Result := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
     End;
  end;
  10: begin // CtrlQueue.DoAllQueue
     If ActiveCircuit[ActiveActor] <> Nil then Begin
        ActiveCircuit[ActiveActor].ControlQueue.DoAllActions(ActiveActor);
     End;
  end
  else
      Result:=-1;
  end;
end;

procedure CtrlQueueV(mode:longint; var myPointer: Pointer; var myType, mySize: longint);cdecl;
Var
  Hour,
  ActionCode,
  DeviceHandle,
  Seconds   : Double;
  pDbl      : ^Double;
  i,
  Qsize     : integer;
Begin
  case mode of
  0: begin  // CtrlQueue.ClearQueue
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    QSize   := ActiveCircuit[ActiveActor].ControlQueue.QueueSize;
    if QSize > 0 then
    begin
      WriteStr2Array('Handle, Hour, Sec, ActionCode, ProxyDevRef, Device');
      WriteStr2Array(Char(0));
      For i := 0 to QSize-1 do
        Begin
          WriteStr2Array(ActiveCircuit[ActiveActor].ControlQueue.QueueItem(i));
          WriteStr2Array(Char(0));
        End;
    end
    else WriteStr2Array('No events');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
  1: begin  // CtrlQueue.Push
    myType  :=  2;        // Double
    pDbl    :=  myPointer;
    QSize   :=  0;
    If ActiveCircuit[ActiveActor] <> Nil then
    Begin
      try
      Begin
        Hour        :=  pDBL^;
        inc(pByte(pDBL), 8);
        Seconds     :=  pDBL^;
        inc(pByte(pDBL), 8);
        ActionCode  :=  pDBL^;
        inc(pByte(pDBL), 8);
        DeviceHandle:=  pDBL^;
        QSize       := ActiveCircuit[ActiveActor].ControlQueue.push(trunc(Hour), Seconds, trunc(ActionCode), trunc(DeviceHandle), COMControlProxyObj, ActiveActor);
      End;
      Except
        Qsize       :=  -10001;              // Something went wrong;
      end;
    End;
    mySize  :=  QSize;
  end
  else
    myType  :=  4;        // String
    setlength(myStrArray, 0);
    WriteStr2Array('Error, parameter not recognized');
    myPointer :=  @(myStrArray[0]);
    mySize    :=  Length(myStrArray);
  end;
End;

initialization
//  writeln(format ('init %s:%s', [{$I %FILE%}, {$I %LINE%}]));
 {Make a Proxy Control Object to receiving control actions}
  Try
    COMControlProxyObj := TCOMControlProxyObj.Create(Nil, 'COM_Proxy');
    ActiveAction := Nil;
  Except
    On E:Exception do DumpExceptionCallStack (E);
  end;
end.
