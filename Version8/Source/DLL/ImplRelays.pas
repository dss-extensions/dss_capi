unit ImplRelays;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TRelays = class(TAutoObject, IRelays)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;

    end;

implementation

uses
    ComServ,
    Executive,
    Relay,
    Circuit,
    DSSGlobals,
    Sysutils,
    Pointerlist,
    Variants;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Relay.%s.%s=%s', [TRelayObj(RelayClass.GetActiveObj).Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TRelays.Get_AllNames: Olevariant;
var
    elem: TRelayObj;
    pList: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if RelayClass.ElementList.ListSize > 0 then
        begin
            pList := RelayClass.ElementList;
            VarArrayRedim(Result, pList.ListSize - 1);
            k := 0;
            elem := pList.First;
            while elem <> NIL do
            begin
                Result[k] := elem.Name;
                Inc(k);
                elem := pList.next;
            end;
        end;
    end;

end;

function TRelays.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := RelayClass.ElementList.ListSize;
end;

function TRelays.Get_First: Integer;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := RelayClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := RelayClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;


function TRelays.Get_Name: Widestring;
var
    elem: TRelayObj;
begin
    Result := '';
    elem := RelayClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.Name;
end;

function TRelays.Get_Next: Integer;
var
    pElem: TRelayObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pElem := RelayClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pElem;
                    Result := RelayClass.ElementList.ActiveIndex;
                end
                else
                    pElem := RelayClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;

procedure TRelays.Set_Name(const Value: Widestring);
// Set element active by name

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if RelayClass.SetActive(Value) then
        begin
            ActiveCircuit[ActiveActor].ActiveCktElement := RelayClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Relay "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;

function TRelays.Get_MonitoredObj: Widestring;
var
    elem: TRelayObj;
begin
    Result := '';
    elem := RelayClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.MonitoredElementName;
end;

procedure TRelays.Set_MonitoredObj(const Value: Widestring);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('monitoredObj', Value);
end;

function TRelays.Get_MonitoredTerm: Integer;
var
    elem: TRelayObj;
begin
    Result := 0;
    elem := RelayClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.MonitoredElementTerminal;
end;

function TRelays.Get_SwitchedObj: Widestring;
var
    elem: TRelayObj;
begin
    Result := '';
    elem := RelayClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.ElementName;

end;


procedure TRelays.Set_MonitoredTerm(Value: Integer);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('monitoredterm', IntToStr(Value));

end;

procedure TRelays.Set_SwitchedObj(const Value: Widestring);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('SwitchedObj', Value);

end;

function TRelays.Get_SwitchedTerm: Integer;
var
    elem: TRelayObj;
begin
    Result := 0;
    elem := RelayClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;

procedure TRelays.Set_SwitchedTerm(Value: Integer);
var
    elem: TRelayObj;
begin
    elem := RelayClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('SwitchedTerm', IntToStr(Value));
end;

function TRelays.Get_idx: Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := RelayClass.ElementList.ActiveIndex
    else
        Result := 0;
end;

procedure TRelays.Set_idx(Value: Integer);
var
    pRelay: TRelayObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pRelay := Relayclass.Elementlist.Get(Value);
        if pRelay <> NIL then
            ActiveCircuit[ActiveActor].ActiveCktElement := pRelay;
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TRelays, Class_Relays,
        ciInternal, tmApartment);
end.
