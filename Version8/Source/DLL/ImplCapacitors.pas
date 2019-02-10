unit ImplCapacitors;

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
    TCapacitors = class(TAutoObject, ICapacitors)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        function Get_kV: Double; SAFECALL;
        function Get_kvar: Double; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_NumSteps: Integer; SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        procedure Set_NumSteps(Value: Integer); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function AddStep: Wordbool; SAFECALL;
        function SubtractStep: Wordbool; SAFECALL;
        function Get_AvailableSteps: Integer; SAFECALL;
        function Get_States: Olevariant; SAFECALL;
        procedure Set_States(Value: Olevariant); SAFECALL;
        procedure Open; SAFECALL;
        procedure Close; SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    Capacitor,
    Variants,
    SysUtils,
    PointerList;

function ActiveCapacitor: TCapacitorObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ShuntCapacitors.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capacitor.%s.%s=%s', [ActiveCapacitor.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TCapacitors.Get_AllNames: Olevariant;
var
    elem: TCapacitorObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ShuntCapacitors.ListSize > 0 then
            begin
                lst := ShuntCapacitors;
                VarArrayRedim(Result, lst.ListSize - 1);
                k := 0;
                elem := lst.First;
                while elem <> NIL do
                begin
                    Result[k] := elem.Name;
                    Inc(k);
                    elem := lst.Next;
                end;
            end;
end;

function TCapacitors.Get_First: Integer;
var
    elem: TCapacitorObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;

function TCapacitors.Get_IsDelta: Wordbool;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    elem := ActiveCapacitor;
    if elem <> NIL then
        if elem.Connection > 0 then
            Result := TRUE;
end;

function TCapacitors.Get_kV: Double;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.NomKV;
end;

function TCapacitors.Get_kvar: Double;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.Totalkvar;
end;

function TCapacitors.Get_Name: Widestring;
var
    elem: TCapacitorObj;
begin
    Result := '';
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.Name;
end;

function TCapacitors.Get_Next: Integer;
var
    elem: TCapacitorObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;

function TCapacitors.Get_NumSteps: Integer;
var
    elem: TCapacitorObj;
begin
    Result := 0;
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.NumSteps;
end;

procedure TCapacitors.Set_IsDelta(Value: Wordbool);
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        elem.Connection := Integer(Value);
end;

procedure TCapacitors.Set_kV(Value: Double);
begin
    Set_Parameter('kv', FloatToStr(Value));
end;

procedure TCapacitors.Set_kvar(Value: Double);
begin
    Set_Parameter('kvar', FloatToStr(Value));
end;

procedure TCapacitors.Set_Name(const Value: Widestring);
var
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    elem: TCapacitorObj;
    lst: TPointerList;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].ShuntCapacitors;
        S := Value;  // Convert to Pascal String
        Found := FALSE;
        ActiveSave := lst.ActiveIndex;
        elem := lst.First;
        while elem <> NIL do
        begin
            if (CompareText(elem.Name, S) = 0) then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                Found := TRUE;
                Break;
            end;
            elem := lst.Next;
        end;
        if not Found then
        begin
            DoSimpleMsg('Capacitor "' + S + '" Not Found in Active Circuit.', 5003);
            elem := lst.Get(ActiveSave);    // Restore active Capacitor
            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        end;
    end;
end;

procedure TCapacitors.Set_NumSteps(Value: Integer);
begin
    Set_Parameter('numsteps', IntToStr(Value));
end;

function TCapacitors.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].ShuntCapacitors.ListSize;
end;

function TCapacitors.AddStep: Wordbool;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.AddStep(ActiveActor);
end;

function TCapacitors.SubtractStep: Wordbool;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.SubtractStep(ActiveActor);

end;

function TCapacitors.Get_AvailableSteps: Integer;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.AvailableSteps;
end;

function TCapacitors.Get_States: Olevariant;
var
    elem: TCapacitorObj;
    i, k: Integer;
begin
    Result := VarArrayCreate([0, 0], varInteger);
    Result[0] := -1;     // error code
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Elem := ActiveCapacitor;
        if Elem <> NIL then
        begin
            VarArrayRedim(Result, elem.NumSteps - 1);
            k := 0;
            for i := 1 to elem.Numsteps do
            begin
                Result[k] := elem.States[i, ActiveActor];
                Inc(k);
            end;
        end;
    end;

end;

procedure TCapacitors.Set_States(Value: Olevariant);
var
    elem: TCapacitorObj;
    i, k, LoopLimit: Integer;

begin
    elem := ActiveCapacitor;
    if elem <> NIL then
    begin
         // allocate space based on present value of NumSteps
         // setting NumSteps allocates the memory
         // only put as many elements as proviced up to nZIPV

        LoopLimit := VarArrayHighBound(Value, 1);
        if (LoopLimit - VarArrayLowBound(Value, 1) + 1) > elem.NumSteps then
            LoopLimit := VarArrayLowBound(Value, 1) + elem.NumSteps - 1;

        k := 1;
        for i := VarArrayLowBound(Value, 1) to LoopLimit do
        begin
            elem.States[k, ActiveActor] := Value[i];
            inc(k);
        end;

        elem.FindLastStepInService;
    end;

end;

procedure TCapacitors.Open;
// Open all steps of capacitor
var
    elem: TCapacitorObj;
    i: Integer;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            elem := ActiveCapacitor;
            if elem <> NIL then
                with elem do
                begin
                    for i := 1 to NumSteps do
                        States[i, ActiveActor] := 0;   // open all steps
                end;
        end;

end;

procedure TCapacitors.Close;
var
    elem: TCapacitorObj;
    i: Integer;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            elem := ActiveCapacitor;
            if elem <> NIL then
                with elem do
                begin
                    ActiveTerminal := Terminals^[1];  // make sure terminal 1 is closed
                    Closed[0, ActiveActor] := TRUE;    // closes all phases
                    for i := 1 to NumSteps do
                        States[i, ActiveActor] := 1;
                end;
        end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TCapacitors, Class_Capacitors,
        ciInternal, tmApartment);
end.
