unit CAPI_Capacitors;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Capacitors_Get_AllNames_GR(); CDECL;
function Capacitors_Get_First(): Integer; CDECL;
function Capacitors_Get_IsDelta(): TAPIBoolean; CDECL;
function Capacitors_Get_kV(): Double; CDECL;
function Capacitors_Get_kvar(): Double; CDECL;
function Capacitors_Get_Name(): PAnsiChar; CDECL;
function Capacitors_Get_Next(): Integer; CDECL;
function Capacitors_Get_NumSteps(): Integer; CDECL;
procedure Capacitors_Set_IsDelta(Value: TAPIBoolean); CDECL;
procedure Capacitors_Set_kV(Value: Double); CDECL;
procedure Capacitors_Set_kvar(Value: Double); CDECL;
procedure Capacitors_Set_Name(const Value: PAnsiChar); CDECL;
procedure Capacitors_Set_NumSteps(Value: Integer); CDECL;
function Capacitors_Get_Count(): Integer; CDECL;
function Capacitors_AddStep(): TAPIBoolean; CDECL;
function Capacitors_SubtractStep(): TAPIBoolean; CDECL;
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
    DSSPointerList,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TCapacitorObj;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TCapacitorObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.ShuntCapacitors.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Capacitor'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: String); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.ParsePropertyValue(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Double); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetDouble(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Integer); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetInteger(idx, val);
end;
//------------------------------------------------------------------------------
procedure Capacitors_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;

    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.ShuntCapacitors, False);
end;

procedure Capacitors_Get_AllNames_GR(); CDECL;
// Same as Capacitors_Get_AllNames but uses global result (GR) pointers
begin
    Capacitors_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Capacitors_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.ShuntCapacitors);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.ShuntCapacitors);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_IsDelta(): TAPIBoolean; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := (elem.Connection = TCapacitorConnection.Delta);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kV(): Double; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NomKV;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_kvar(): Double; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Totalkvar;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Name(): PAnsiChar; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_NumSteps(): Integer; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NumSteps;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_IsDelta(Value: TAPIBoolean); CDECL;
var
    elem: TCapacitorObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if Value then
        elem.Connection := TCapacitorConnection.Delta
    else
        elem.Connection := TCapacitorConnection.Wye;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_kV(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TCapacitorProp.kv), Value);
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_kvar(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TCapacitorProp.kvar), Value);
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.CapacitorClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.CapacitorClass.ElementList.Active;
        DSSPrime.ActiveCircuit.ShuntCapacitors.Get(DSSPrime.CapacitorClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Capacitor "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_NumSteps(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TCapacitorProp.NumSteps), Value);
end;
//------------------------------------------------------------------------------
function Capacitors_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ShuntCapacitors.Count;
end;
//------------------------------------------------------------------------------
function Capacitors_AddStep(): TAPIBoolean; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.AddStep();
end;
//------------------------------------------------------------------------------
function Capacitors_SubtractStep(): TAPIBoolean; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.SubtractStep();
end;
//------------------------------------------------------------------------------
function Capacitors_Get_AvailableSteps(): Integer; CDECL;
var
    elem: TCapacitorObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.AvailableSteps;
end;
//------------------------------------------------------------------------------
procedure Capacitors_Get_States(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray0;
    elem: TCapacitorObj;
begin
    if not _activeObj(DSSPrime, elem) then
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
    Capacitors_Get_States(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
procedure Capacitors_Set_States(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
var
    Value: PIntegerArray;
    elem: TCapacitorObj;
    i, LoopLimit: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (ValueCount <> elem.NumSteps) and DSS_CAPI_EXT_ERRORS then
    begin
        DoSimpleMsg(DSSPrime, 'The number of states provided (%d) does not match the number of steps (%d) in the active capacitor.', 
            [ValueCount, elem.NumSteps], 
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
    if not _activeObj(DSSPrime, elem) then
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
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    with elem do
    begin
        //TODO: why is this changing ActiveTerminal directly?
        ActiveTerminal := @Terminals[0];  // make sure terminal 1 is closed
        Closed[0] := TRUE;    // closes all phases
        for i := 1 to NumSteps do
            States[i] := 1;
    end;
end;
//------------------------------------------------------------------------------
function Capacitors_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ShuntCapacitors.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Capacitors_Set_idx(Value: Integer); CDECL;
var
    pCapacitor: TCapacitorObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    pCapacitor := DSSPrime.ActiveCircuit.ShuntCapacitors.Get(Value);
    if pCapacitor = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Capacitor index: "%d".', [Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pCapacitor;
end;
//------------------------------------------------------------------------------
end.
