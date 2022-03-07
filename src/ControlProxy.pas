unit ControlProxy;

{Moved from CAPI_CtrlQueue.pas to avoid global variables}

interface

uses 
    Classes,
    ControlQueue,
    ControlElem,
    DSSClass,
    sysutils,
    Utilities,
    UComplex, DSSUcomplex;

type
    TControlProxyObj = class(TControlElem)
    PUBLIC
        ActionList: TList;
        procedure ClearActionList;
        function PopAction: Boolean;

        constructor Create(context: TDSSContext);
        destructor Destroy; OVERRIDE;

        procedure DoPendingAction(const Code, ProxyHdl: Integer); OVERRIDE;   // Do the action that is pending from last sample
        procedure Reset; OVERRIDE;  // Reset to initial defined state
        procedure RecalcElementData; Override;
    end;

implementation

uses 
    DSSGlobals,
    DSSClassDefs;

procedure TControlProxyObj.DoPendingAction(const Code, ProxyHdl: Integer);
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

procedure TControlProxyObj.RecalcElementData;
begin
    raise Exception.Create('This procedure should not be called');
end;

procedure TControlProxyObj.ClearActionList;
begin
    while PopAction do ;  // spin until it is done
end;

constructor TControlProxyObj.Create(context: TDSSContext);
begin
    DSS := context;
    Name := 'COM_Proxy';
    ActionList := TList.Create;
    DSSObjType := DSS_OBJECT + HIDDEN_ELEMENT;
    
    pUuid := nil;
end;

destructor TControlProxyObj.Destroy;
begin
    ClearActionList;
    ActionList.Free;
    DSS := nil;
    inherited;
end;

function TControlProxyObj.PopAction: Boolean;
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

procedure TControlProxyObj.Reset;
begin
    ClearActionList;
end;

end.