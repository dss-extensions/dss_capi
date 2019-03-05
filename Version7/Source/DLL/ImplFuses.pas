unit ImplFuses;

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
    TFuses = class(TAutoObject, IFuses)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_TCCcurve: Widestring; SAFECALL;
        procedure Set_TCCcurve(const Value: Widestring); SAFECALL;
        function Get_RatedCurrent: Double; SAFECALL;
        procedure Set_RatedCurrent(Value: Double); SAFECALL;
        function Get_Delay: Double; SAFECALL;
        procedure Open; SAFECALL;
        procedure Close; SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        function IsBlown: Wordbool; STDCALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        function Get_NumPhases: Integer; SAFECALL;

    end;

implementation

uses
    ComServ,
    Executive,
    Sysutils,
    Fuse,
    Pointerlist,
    DSSGlobals,
    Variants;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Fuse.%s.%s=%s', [TFuseObj(FuseClass.GetActiveObj).Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TFuses.Get_AllNames: Olevariant;
var
    elem: TFuseObj;
    pList: TPointerList;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
    begin
        if FuseClass.ElementList.ListSize > 0 then
        begin
            pList := FuseClass.ElementList;
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

function TFuses.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := FuseClass.ElementList.ListSize;
end;

function TFuses.Get_First: Integer;
var
    pElem: TFuseObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := FuseClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := FuseClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;

function TFuses.Get_Name: Widestring;
var
    elem: TFuseObj;
begin
    Result := '';
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.Name;
end;

function TFuses.Get_Next: Integer;
var
    pElem: TFuseObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := FuseClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := FuseClass.ElementList.ActiveIndex;
                end
                else
                    pElem := FuseClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;

procedure TFuses.Set_Name(const Value: Widestring);
// Set element active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if FuseClass.SetActive(Value) then
        begin
            ActiveCircuit.ActiveCktElement := FuseClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Fuse "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;

function TFuses.Get_MonitoredObj: Widestring;
var
    elem: TFuseObj;
begin
    Result := '';
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.MonitoredElementName;
end;

function TFuses.Get_MonitoredTerm: Integer;
var
    elem: TFuseObj;
begin
    Result := 0;
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.MonitoredElementTerminal;
end;

function TFuses.Get_SwitchedObj: Widestring;
var
    elem: TFuseObj;
begin
    Result := '';
    elem := FuseClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.ElementName;
end;

procedure TFuses.Set_MonitoredObj(const Value: Widestring);
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('monitoredObj', Value);
end;

procedure TFuses.Set_MonitoredTerm(Value: Integer);
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('monitoredterm', IntToStr(Value));
end;

procedure TFuses.Set_SwitchedObj(const Value: Widestring);
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('SwitchedObj', Value);
end;

function TFuses.Get_SwitchedTerm: Integer;
var
    elem: TFuseObj;
begin
    Result := 0;
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;

procedure TFuses.Set_SwitchedTerm(Value: Integer);
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('SwitchedTerm', IntToStr(Value));
end;

function TFuses.Get_TCCcurve: Widestring;
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.FuseCurve.Name
    else
        Result := 'No Fuse Active!';
end;

procedure TFuses.Set_TCCcurve(const Value: Widestring);
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('FuseCurve', Value);
end;

function TFuses.Get_RatedCurrent: Double;
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.RatedCurrent
    else
        Result := -1.0;
end;

procedure TFuses.Set_RatedCurrent(Value: Double);
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('RatedCurrent', Format('%.8g ', [Value]));
end;

function TFuses.Get_Delay: Double;
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.DelayTime
    else
        Result := -1.0;
end;

procedure TFuses.Open;
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        elem.ControlledElement.Closed[0] := FALSE; // Open all phases
end;

procedure TFuses.Close;
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        elem.Reset;
end;

procedure TFuses.Set_Delay(Value: Double);
var
    elem: TFuseObj;
begin
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('Delay', Format('%.8g ', [Value]));
end;

function TFuses.IsBlown: Wordbool;

// Return TRUE if any phase blown
var
    elem: TFuseObj;
    i: Integer;
begin
    Result := FALSE;
    elem := FuseClass.GetActiveObj;
    if elem <> NIL then
    begin
        for i := 1 to elem.nphases do
            if not elem.ControlledElement.Closed[i] then
                Result := TRUE;
    end;
end;

function TFuses.Get_idx: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := FuseClass.ElementList.ActiveIndex
    else
        Result := 0;
end;

procedure TFuses.Set_idx(Value: Integer);
var
    pFuse: TFuseObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pFuse := FuseClass.Elementlist.Get(Value);
        if pFuse <> NIL then
            ActiveCircuit.ActiveCktElement := pFuse;
    end;
end;

function TFuses.Get_NumPhases: Integer;
var
    pFuse: TFuseObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pFuse := FuseClass.GetActiveObj;
        if pFuse <> NIL then
            Result := pFuse.NPhases;
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TFuses, Class_Fuses,
        ciInternal, tmApartment);
end.
