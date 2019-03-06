unit CAPI_Capacitors;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Capacitors_Get_AllNames_GR(); CDECL;
function Capacitors_Get_First(): Integer; CDECL;
function Capacitors_Get_IsDelta(): Wordbool; CDECL;
function Capacitors_Get_kV(): Double; CDECL;
function Capacitors_Get_kvar(): Double; CDECL;
function Capacitors_Get_Name(): PAnsiChar; CDECL;
function Capacitors_Get_Next(): Integer; CDECL;
function Capacitors_Get_NumSteps(): Integer; CDECL;
procedure Capacitors_Set_IsDelta(Value: Wordbool); CDECL;
procedure Capacitors_Set_kV(Value: Double); CDECL;
procedure Capacitors_Set_kvar(Value: Double); CDECL;
procedure Capacitors_Set_Name(const Value: PAnsiChar); CDECL;
procedure Capacitors_Set_NumSteps(Value: Integer); CDECL;
function Capacitors_Get_Count(): Integer; CDECL;
function Capacitors_AddStep(): Wordbool; CDECL;
function Capacitors_SubtractStep(): Wordbool; CDECL;
function Capacitors_Get_AvailableSteps(): Integer; CDECL;
procedure Capacitors_Get_States(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure Capacitors_Get_States_GR(); CDECL;
procedure Capacitors_Set_States(ValuePtr: PInteger; ValueCount: Integer); CDECL;
procedure Capacitors_Open(); CDECL;
procedure Capacitors_Close(); CDECL;

// API Extensions
function Capacitors_Get_idx(): Integer; CDECL;
procedure Capacitors_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    Capacitor,
    SysUtils,
    PointerList;

function ActiveCapacitor: TCapacitorObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].ShuntCapacitors.Active;
end;
//------------------------------------------------------------------------------
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
//------------------------------------------------------------------------------
procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    elem: TCapacitorObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ShuntCapacitors.ListSize > 0 then
            begin
                lst := ShuntCapacitors;
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (lst.ListSize - 1) + 1);
                k := 0;
                elem := lst.First;
                while elem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(elem.Name);
                    Inc(k);
                    elem := lst.Next;
                end;
            end;
end;

procedure Capacitors_Get_AllNames_GR(); CDECL;
// Same as Capacitors_Get_AllNames but uses global result (GR) pointers
begin
    Capacitors_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Capacitors_Get_First(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function Capacitors_Get_IsDelta(): Wordbool; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    elem := ActiveCapacitor;
    if elem <> NIL then
        if elem.Connection > 0 then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kV(): Double; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.NomKV;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kvar(): Double; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.Totalkvar;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TCapacitorObj;
begin
    Result := '';
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.Name;
end;

function Capacitors_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Capacitors_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Next(): Integer; CDECL;
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
//------------------------------------------------------------------------------
function Capacitors_Get_NumSteps(): Integer; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0;
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.NumSteps;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_IsDelta(Value: Wordbool); CDECL;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        elem.Connection := Integer(Value);
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_kV(Value: Double); CDECL;
begin
    Set_Parameter('kv', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_kvar(Value: Double); CDECL;
begin
    Set_Parameter('kvar', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_Name(const Value: PAnsiChar); CDECL;
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
//------------------------------------------------------------------------------
procedure Capacitors_Set_NumSteps(Value: Integer); CDECL;
begin
    Set_Parameter('numsteps', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Count(): Integer; CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].ShuntCapacitors.ListSize;
end;
//------------------------------------------------------------------------------
function Capacitors_AddStep(): Wordbool; CDECL;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.AddStep(ActiveActor);
end;
//------------------------------------------------------------------------------
function Capacitors_SubtractStep(): Wordbool; CDECL;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.SubtractStep(ActiveActor);

end;
//------------------------------------------------------------------------------
function Capacitors_Get_AvailableSteps(): Integer; CDECL;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.AvailableSteps;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Get_States(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    elem: TCapacitorObj;
    i, k: Integer;
begin
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, (0) + 1);
    Result[0] := -1;     // error code
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        Elem := ActiveCapacitor;
        if Elem <> NIL then
        begin
            DSS_RecreateArray_PInteger(Result, ResultPtr, ResultCount, (elem.NumSteps - 1) + 1);
            k := 0;
            for i := 1 to elem.Numsteps do
            begin
                Result[k] := elem.States[i, ActiveActor];
                Inc(k);
            end;
        end;
    end;

end;

procedure Capacitors_Get_States_GR(); CDECL;
// Same as Capacitors_Get_States but uses global result (GR) pointers
begin
    Capacitors_Get_States(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Capacitors_Set_States(ValuePtr: PInteger; ValueCount: Integer); CDECL;
var
    Value: PIntegerArray;
    elem: TCapacitorObj;
    i, k, LoopLimit: Integer;

begin
    Value := PIntegerArray(ValuePtr);
    elem := ActiveCapacitor;
    if elem <> NIL then
    begin
         // allocate space based on present value of NumSteps
         // setting NumSteps allocates the memory
         // only put as many elements as proviced up to nZIPV

        LoopLimit := (ValueCount - 1);
        if (LoopLimit - (0) + 1) > elem.NumSteps then
            LoopLimit := (0) + elem.NumSteps - 1;

        k := 1;
        for i := (0) to LoopLimit do
        begin
            elem.States[k, ActiveActor] := Value[i];
            inc(k);
        end;

        elem.FindLastStepInService;
    end;

end;
//------------------------------------------------------------------------------
procedure Capacitors_Open(); CDECL;
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
//------------------------------------------------------------------------------
procedure Capacitors_Close(); CDECL;
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
//------------------------------------------------------------------------------
function Capacitors_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Result := ActiveCircuit[ActiveActor].ShuntCapacitors.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_idx(Value: Integer); CDECL;
var
    pCapacitor: TCapacitorObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pCapacitor := ActiveCircuit[ActiveActor].ShuntCapacitors.Get(Value);
    if pCapacitor = NIL then
        Exit;
    ActiveCircuit[ActiveActor].ActiveCktElement := pCapacitor;
end;
//------------------------------------------------------------------------------
end.
