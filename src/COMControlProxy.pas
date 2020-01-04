unit COMControlProxy;

{Moved from CAPI_CtrlQueue.pas to avoid global variables}

interface

uses 
    Classes,
    DSSGlobals,
    ControlQueue,
    ControlElem,
    DSSClass,
    sysutils,
    Utilities;

type
    TCOMControlProxyObj = class(TControlElem)
    PUBLIC
        ActionList: TList;
        procedure ClearActionList;
        function PopAction: Boolean;

        constructor Create(context: TDSSContext);
        destructor Destroy; OVERRIDE;

        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state
    end;

implementation

uses 
    DSSClassDefs;

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

procedure TCOMControlProxyObj.ClearActionList;
begin
    while PopAction do ;  // spin until it is done
end;

constructor TCOMControlProxyObj.Create(context: TDSSContext);
begin
    DSS := context;
    Name := 'COM_Proxy';
    ActionList := TList.Create;
    DSSObjType := DSS_OBJECT + HIDDEN_ELEMENT;
    
    pUuid := nil;
end;

destructor TCOMControlProxyObj.Destroy;
begin
    ClearActionList;
    ActionList.Free;
    DSS := nil;
    inherited;
end;

function TCOMControlProxyObj.PopAction: Boolean;
begin
    if DSS.ActiveAction <> NIL then
    begin
        Freemem(DSS.ActiveAction, Sizeof(TAction));
        DSS.ActiveAction := NIL;
    end;
    Result := TRUE;
    if ActionList.Count > 0 then
    begin
        DSS.ActiveAction := ActionList.Items[0];
        ActionList.Delete(0);
    end
    else
        Result := FALSE;
end;

procedure TCOMControlProxyObj.Reset;
begin
    ClearActionList;
end;

end.