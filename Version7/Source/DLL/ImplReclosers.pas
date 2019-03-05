unit ImplReclosers;

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
    TReclosers = class(TAutoObject, IReclosers)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        function Get_SwitchedObj: Widestring; SAFECALL;
        procedure Set_SwitchedObj(const Value: Widestring); SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        function Get_SwitchedTerm: Integer; SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        procedure Set_SwitchedTerm(Value: Integer); SAFECALL;
        function Get_NumFast: Integer; SAFECALL;
        function Get_RecloseIntervals: Olevariant; SAFECALL;
        function Get_Shots: Integer; SAFECALL;
        procedure Set_NumFast(Value: Integer); SAFECALL;
        procedure Set_Shots(Value: Integer); SAFECALL;
        function Get_PhaseTrip: Double; SAFECALL;
        procedure Set_PhaseTrip(Value: Double); SAFECALL;
        function Get_GroundInst: Double; SAFECALL;
        function Get_GroundTrip: Double; SAFECALL;
        function Get_PhaseInst: Double; SAFECALL;
        procedure Set_GroundInst(Value: Double); SAFECALL;
        procedure Set_GroundTrip(Value: Double); SAFECALL;
        procedure Set_PhaseInst(Value: Double); SAFECALL;
        procedure Close; SAFECALL;
        procedure Open; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;

    end;

implementation

uses
    ComServ,
    Executive,
    Sysutils,
    Recloser,
    PointerList,
    Variants,
    DSSGlobals,
    DSSClassDefs;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('recloser.%s.%s=%s', [TRecloserObj(RecloserClass.GetActiveObj).Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TReclosers.Get_AllNames: Olevariant;
var
    elem: TRecloserObj;
    pList: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
    begin
        if RecloserClass.ElementList.ListSize > 0 then
        begin
            pList := RecloserClass.ElementList;
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

function TReclosers.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := RecloserClass.ElementList.ListSize;
end;

function TReclosers.Get_First: Integer;
var
    pElem: TRecloserObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := RecloserClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := RecloserClass.ElementList.ActiveIndex;
                end
                else
                    pElem := RecloserClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;

function TReclosers.Get_Name: Widestring;
var
    elem: TRecloserObj;
begin
    Result := '';
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.Name;
end;

function TReclosers.Get_Next: Integer;
var
    pElem: TRecloserObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := RecloserClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := RecloserClass.ElementList.ActiveIndex;
                end
                else
                    pElem := RecloserClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;

procedure TReclosers.Set_Name(const Value: Widestring);
// Set element active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if RecloserClass.SetActive(Value) then
        begin
            ActiveCircuit.ActiveCktElement := RecloserClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Recloser "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;

function TReclosers.Get_MonitoredObj: Widestring;
var
    elem: TRecloserObj;
begin
    Result := '';
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.MonitoredElementName;
end;

procedure TReclosers.Set_MonitoredObj(const Value: Widestring);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('monitoredObj', Value);
end;

function TReclosers.Get_MonitoredTerm: Integer;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.MonitoredElementTerminal;
end;

procedure TReclosers.Set_MonitoredTerm(Value: Integer);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('monitoredterm', IntToStr(Value));
end;

function TReclosers.Get_SwitchedObj: Widestring;
var
    elem: TRecloserObj;
begin
    Result := '';
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.ElementName;
end;

procedure TReclosers.Set_SwitchedObj(const Value: Widestring);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('SwitchedObj', Value);
end;

function TReclosers.Get_SwitchedTerm: Integer;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;

procedure TReclosers.Set_SwitchedTerm(Value: Integer);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.GetActiveObj;
    if elem <> NIL then
        Set_parameter('SwitchedTerm', IntToStr(Value));
end;

function TReclosers.Get_NumFast: Integer;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.ElementList.Active;
    ;
    if elem <> NIL then
        Result := elem.NumFast;
end;

function TReclosers.Get_RecloseIntervals: Olevariant;
// return reclose intervals in seconds
var
    elem: TRecloserObj;
    i, k: Integer;
begin
    Result := VarArrayCreate([0, 0], varDouble);
    Result[0] := -1.0;
    if ActiveCircuit <> NIL then
    begin
        elem := RecloserClass.ElementList.Active;
        if elem <> NIL then
        begin
            VarArrayRedim(Result, elem.NumReclose - 1);
            k := 0;
            for i := 1 to elem.NumReclose do
            begin
                Result[k] := elem.RecloseIntervals^[i];
                Inc(k);
            end;
        end;
    end;
end;

function TReclosers.Get_Shots: Integer;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.ElementList.Active;
    ;
    if elem <> NIL then
        Result := elem.NumReclose + 1;
end;

procedure TReclosers.Set_NumFast(Value: Integer);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('numfast', IntToStr(Value));
end;

procedure TReclosers.Set_Shots(Value: Integer);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('shots', IntToStr(Value));
end;

function TReclosers.Get_PhaseTrip: Double;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.PhaseTrip;
end;

procedure TReclosers.Set_PhaseTrip(Value: Double);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('PhaseTrip', Format('%.g', [Value]));
end;

function TReclosers.Get_GroundInst: Double;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.GroundInst;
end;

function TReclosers.Get_GroundTrip: Double;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.GroundTrip;
end;

function TReclosers.Get_PhaseInst: Double;
var
    elem: TRecloserObj;
begin
    Result := 0;
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.PhaseInst;
end;

procedure TReclosers.Set_GroundInst(Value: Double);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('GroundInst', Format('%.g', [Value]));
end;

procedure TReclosers.Set_GroundTrip(Value: Double);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('GroundTrip', Format('%.g', [Value]));
end;

procedure TReclosers.Set_PhaseInst(Value: Double);
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('Phaseinst', Format('%.g', [Value]));
end;

procedure TReclosers.Close;
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('Action', 'close');
end;

procedure TReclosers.Open;
var
    elem: TRecloserObj;
begin
    elem := RecloserClass.ElementList.Active;
    if elem <> NIL then
        Set_parameter('Action', 'open');
end;

function TReclosers.Get_idx: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := RecloserClass.ElementList.ActiveIndex
    else
        Result := 0;
end;

procedure TReclosers.Set_idx(Value: Integer);
var
    pRecloser: TRecloserObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pRecloser := RecloserClass.Elementlist.Get(Value);
        if pRecloser <> NIL then
            ActiveCircuit.ActiveCktElement := pRecloser;
    end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TReclosers, Class_Reclosers,
        ciInternal, tmApartment);
end.
