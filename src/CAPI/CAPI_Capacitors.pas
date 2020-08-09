unit CAPI_Capacitors;

interface

uses
    CAPI_Utils;

procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
procedure Capacitors_Get_States(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Capacitors_Get_States_GR(); CDECL;
procedure Capacitors_Set_States(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
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

//------------------------------------------------------------------------------
function _activeObj(out obj: TCapacitorObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.ShuntCapacitors.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Capacitor object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    elem: TCapacitorObj;
begin
    if not _activeObj(elem) then
        Exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capacitor.%s.%s=%s', [elem.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;

    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.ShuntCapacitors, False);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_First(ActiveCircuit.ShuntCapacitors);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := Generic_CktElement_Get_Next(ActiveCircuit.ShuntCapacitors);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_IsDelta(): Boolean; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := (elem.Connection > 0);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kV(): Double; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.NomKV;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kvar(): Double; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.Totalkvar;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Name(): PAnsiChar; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_NumSteps(): Integer; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.NumSteps;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_IsDelta(Value: Boolean); CDECL;
var
    elem: TCapacitorObj;
begin
    if not _activeObj(elem) then
        Exit;
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
    if InvalidCircuit then
        Exit;

    if CapacitorClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := CapacitorClass.ElementList.Active;
        ActiveCircuit.ShuntCapacitors.Get(CapacitorClass.Active);
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
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.ShuntCapacitors.ListSize;
end;
//------------------------------------------------------------------------------
function Capacitors_AddStep(): Boolean; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := elem.AddStep();
end;
//------------------------------------------------------------------------------
function Capacitors_SubtractStep(): Boolean; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := elem.SubtractStep();
end;
//------------------------------------------------------------------------------
function Capacitors_Get_AvailableSteps(): Integer; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.AvailableSteps;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Get_States(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray;
    elem: TCapacitorObj;
begin
    if not _activeObj(elem) then
    begin
        DefaultResult(ResultPtr, ResultCount, -1);
        Exit;
    end;

    DSS_RecreateArray_PInteger(Result, ResultPtr, ResultCount, elem.NumSteps);
    Move(elem.FStates[1], ResultPtr^, elem.NumSteps * SizeOf(Integer));
end;

procedure Capacitors_Get_States_GR(); CDECL;
// Same as Capacitors_Get_States but uses global result (GR) pointers
begin
    Capacitors_Get_States(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Capacitors_Set_States(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
var
    Value: PIntegerArray;
    elem: TCapacitorObj;
    i, LoopLimit: Integer;
begin
    if not _activeObj(elem) then
        Exit;

    if (ValueCount <> elem.NumSteps) and DSS_CAPI_EXT_ERRORS then
    begin
        DoSimpleMsg(Format('The number of states provided (%d) does not match the number of steps (%d) in the active capacitor.', 
            [ValueCount, elem.NumSteps]), 
            8989
        );
        Exit;
    end;

    Value := PIntegerArray(ValuePtr);
    LoopLimit := ValueCount;
    if LoopLimit > elem.NumSteps then
    begin
        LoopLimit := elem.NumSteps;
    end;

    for i := 1 to LoopLimit do
    begin
        elem.States[i] := Value[i - 1];
    end;

    elem.FindLastStepInService();
end;
//------------------------------------------------------------------------------
procedure Capacitors_Open(); CDECL;
// Open all steps of capacitor
var
    elem: TCapacitorObj;
    i: Integer;
begin
    if not _activeObj(elem) then
        Exit;

    with elem do
        for i := 1 to NumSteps do
            States[i] := 0;   // open all steps
end;
//------------------------------------------------------------------------------
procedure Capacitors_Close(); CDECL;
var
    elem: TCapacitorObj;
    i: Integer;
begin
    if not _activeObj(elem) then
        Exit;
    
    with elem do
    begin
        ActiveTerminal := Terminals^[1];  // make sure terminal 1 is closed
        Closed[0] := TRUE;    // closes all phases
        for i := 1 to NumSteps do
            States[i] := 1;
    end;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.ShuntCapacitors.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_idx(Value: Integer); CDECL;
var
    pCapacitor: TCapacitorObj;
begin
    if InvalidCircuit then
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
