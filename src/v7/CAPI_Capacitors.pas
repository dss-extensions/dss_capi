unit CAPI_Capacitors;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
function Capacitors_Get_First(): Integer; CDECL;
function Capacitors_Get_IsDelta(): Boolean; CDECL;
function Capacitors_Get_kV(): Double; CDECL;
function Capacitors_Get_kvar(): Double; CDECL;
function Capacitors_Get_Name(): PAnsiChar; CDECL;
function Capacitors_Get_Next(): Integer; CDECL;
function Capacitors_Get_NumSteps(): Integer; CDECL;
procedure Capacitors_Set_IsDelta(Value: Boolean); CDECL;
procedure Capacitors_Set_kV(Value: Double); CDECL;
procedure Capacitors_Set_kvar(Value: Double); CDECL;
procedure Capacitors_Set_Name(const Value: PAnsiChar); CDECL;
procedure Capacitors_Set_NumSteps(Value: Integer); CDECL;
function Capacitors_Get_Count(): Integer; CDECL;
function Capacitors_AddStep(): Boolean; CDECL;
function Capacitors_SubtractStep(): Boolean; CDECL;
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
    PointerList,
    DSSClass,
    DSSHelper;

function ActiveCapacitor: TCapacitorObj;
begin
    Result := NIL;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ShuntCapacitors.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    DSSPrime.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capacitor.%s.%s=%s', [ActiveCapacitor.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.ShuntCapacitors, False);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_First(): Integer; CDECL;
var
    elem: TCapacitorObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.ShuntCapacitors;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_IsDelta(): Boolean; CDECL;
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
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.ShuntCapacitors;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
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
procedure Capacitors_Set_IsDelta(Value: Boolean); CDECL;
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
begin
    if ActiveCircuit = NIL then
        Exit;
    if DSSPrime.CapacitorClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := DSSPrime.CapacitorClass.ElementList.Active;
        ActiveCircuit.ShuntCapacitors.Get(DSSPrime.CapacitorClass.Active);
    end
    else
    begin
        DoSimpleMsg('Capacitor "' + Value + '" Not Found in Active Circuit.', 5003);
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
    Result := 0;
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.ShuntCapacitors.ListSize;
end;
//------------------------------------------------------------------------------
function Capacitors_AddStep(): Boolean; CDECL;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.AddStep;
end;
//------------------------------------------------------------------------------
function Capacitors_SubtractStep(): Boolean; CDECL;
var
    elem: TCapacitorObj;
begin
    elem := ActiveCapacitor;
    if elem <> NIL then
        Result := elem.SubtractStep;

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
    if ActiveCircuit <> NIL then
    begin
        Elem := ActiveCapacitor;
        if Elem <> NIL then
        begin
            DSS_RecreateArray_PInteger(Result, ResultPtr, ResultCount, (elem.NumSteps - 1) + 1);
            k := 0;
            for i := 1 to elem.Numsteps do
            begin
                Result[k] := elem.States[i];
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
            elem.States[k] := Value[i];
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

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            elem := ActiveCapacitor;
            if elem <> NIL then
                with elem do
                begin
                    for i := 1 to NumSteps do
                        States[i] := 0;   // open all steps
                end;
        end;

end;
//------------------------------------------------------------------------------
procedure Capacitors_Close(); CDECL;
var
    elem: TCapacitorObj;
    i: Integer;
begin

    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            elem := ActiveCapacitor;
            if elem <> NIL then
                with elem do
                begin
                    ActiveTerminal := Terminals^[1];  // make sure terminal 1 is closed
                    Closed[0] := TRUE;    // closes all phases
                    for i := 1 to NumSteps do
                        States[i] := 1;
                end;
        end;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.ShuntCapacitors.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_idx(Value: Integer); CDECL;
var
    pCapacitor: TCapacitorObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pCapacitor := ActiveCircuit.ShuntCapacitors.Get(Value);
    if pCapacitor = NIL then
    begin
        DoSimpleMsg('Invalid Capacitor index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pCapacitor;
end;
//------------------------------------------------------------------------------
end.
