unit CAPI_Fuses;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Fuses_Get_AllNames_GR(); CDECL;
function Fuses_Get_Count(): Integer; CDECL;
function Fuses_Get_First(): Integer; CDECL;
function Fuses_Get_Name(): PAnsiChar; CDECL;
function Fuses_Get_Next(): Integer; CDECL;
procedure Fuses_Set_Name(const Value: PAnsiChar); CDECL;
function Fuses_Get_MonitoredObj(): PAnsiChar; CDECL;
function Fuses_Get_MonitoredTerm(): Integer; CDECL;
function Fuses_Get_SwitchedObj(): PAnsiChar; CDECL;
procedure Fuses_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
procedure Fuses_Set_MonitoredTerm(Value: Integer); CDECL;
procedure Fuses_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
function Fuses_Get_SwitchedTerm(): Integer; CDECL;
procedure Fuses_Set_SwitchedTerm(Value: Integer); CDECL;
function Fuses_Get_TCCcurve(): PAnsiChar; CDECL;
procedure Fuses_Set_TCCcurve(const Value: PAnsiChar); CDECL;
function Fuses_Get_RatedCurrent(): Double; CDECL;
procedure Fuses_Set_RatedCurrent(Value: Double); CDECL;
function Fuses_Get_Delay(): Double; CDECL;
procedure Fuses_Open(); CDECL;
procedure Fuses_Close(); CDECL;
procedure Fuses_Set_Delay(Value: Double); CDECL;
function Fuses_IsBlown(): TAPIBoolean; CDECL;
function Fuses_Get_idx(): Integer; CDECL;
procedure Fuses_Set_idx(Value: Integer); CDECL;
function Fuses_Get_NumPhases(): Integer; CDECL;
procedure Fuses_Reset(); CDECL;
procedure Fuses_Get_NormalState(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Fuses_Set_NormalState(ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
procedure Fuses_Get_State(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Fuses_Set_State(ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;

implementation

uses
    CAPI_Constants,
    ControlElem,
    Executive,
    Sysutils,
    Fuse,
    DSSPointerlist,
    DSSGlobals,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TFuseObj;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.Fuses.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Fuse'], 8989);
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
procedure Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Fuses, False);
end;

procedure Fuses_Get_AllNames_GR(); CDECL;
// Same as Fuses_Get_AllNames but uses global result (GR) pointers
begin
    Fuses_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Fuses_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Fuses.Count;
end;
//------------------------------------------------------------------------------
function Fuses_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Fuses);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Fuses);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Name(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.FuseClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.FuseClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Fuses.Get(DSSPrime.FuseClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Fuse "%s" not found in Active Circuit.', [Value], 77003);
    end;
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.MonitoredElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(elem.MonitoredElement.FullName));
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.ControlledElement <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(elem.ControlledElement.FullName));
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, ord(TFuseProp.monitoredObj), Value);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredTerm(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, ord(TFuseProp.monitoredterm), Value);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, ord(TFuseProp.SwitchedObj), Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedTerm(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, ord(TFuseProp.SwitchedTerm), Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_TCCcurve(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        Result := DSS_GetAsPAnsiChar(DSSPrime, 'No Fuse Active!');
        Exit;
    end;

    If elem.FuseCurve <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.FuseCurve.Name)
    else
        Result := NIL;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_TCCcurve(const Value: PAnsiChar); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, ord(TFuseProp.FuseCurve), Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_RatedCurrent(): Double; CDECL;
var
    elem: TObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.RatedCurrent
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_RatedCurrent(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, ord(TFuseProp.RatedCurrent), Format('%.8g ', [Value]));
end;
//------------------------------------------------------------------------------
function Fuses_Get_Delay(): Double; CDECL;
var
    elem: TObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.DelayTime;
end;
//------------------------------------------------------------------------------
procedure Fuses_Open(); CDECL;
var
    elem: TObj;
    i: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.ControlledElement.Closed[0] := FALSE; // Open all phases

    // TODO: check why original code doesn't do this (see related issue with GetPropertyValue)
    for i := 1 to elem.ControlledElement.NPhases do 
        elem.FPresentState[i] := CTRL_OPEN; // Open all phases
end;
//------------------------------------------------------------------------------
procedure Fuses_Close(); CDECL;
var
    elem: TObj;
    i: Integer;
begin
    if (not _activeObj(DSSPrime, elem)) or (elem.ControlledElement = NIL) then
        Exit;

    for i := 1 to elem.ControlledElement.NPhases do 
        elem.FPresentState[i] := CTRL_CLOSE; // Close all phases
    elem.PropertySideEffects(ord(TFuseProp.State));
end;
//------------------------------------------------------------------------------
procedure Fuses_Reset(); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.Reset();
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Delay(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, ord(TFuseProp.Delay), Format('%.8g ', [Value]));
end;
//------------------------------------------------------------------------------
function Fuses_IsBlown(): TAPIBoolean; CDECL;
// Return TRUE if any phase blown
var
    elem: TObj;
    i: Integer;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    for i := 1 to elem.nphases do
        if not elem.ControlledElement.Closed[i] then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function Fuses_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Fuses.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_idx(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Get(Value);
    if elem = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Fuse', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := elem;
end;
//------------------------------------------------------------------------------
function Fuses_Get_NumPhases(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
procedure Fuses_Get_NormalState(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    elem: TObj;
    i: Integer;
begin
    if (not _activeObj(DSSPrime, elem)) or (elem.ControlledElement = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, elem.ControlledElement.NPhases);
    for i := 1 to elem.ControlledElement.NPhases do
        if elem.FNormalState[i] = CTRL_CLOSE then 
            Result[i - 1] := 'closed' 
        else 
            Result[i - 1] := 'open';
end;
//------------------------------------------------------------------------------
procedure Fuses_Get_State(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    elem: TObj;
    i: Integer;
begin
    if (not _activeObj(DSSPrime, elem)) or (elem.ControlledElement = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, elem.ControlledElement.NPhases);
    for i := 1 to elem.ControlledElement.NPhases do
        if elem.States[i] = CTRL_CLOSE then 
            Result[i - 1] := 'closed' 
        else 
            Result[i - 1] := 'open';
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_State(ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
var
    Value: PPAnsiCharArray0;
    i: Integer;
    Count: Integer;
    elem: TObj;
begin
    if (not _activeObj(DSSPrime, elem)) or (elem.ControlledElement = NIL) then
        Exit;

    Value := PPAnsiCharArray0(ValuePtr);

    Count := ValueCount;
    if (Count <> elem.ControlledElement.NPhases) AND (DSS_CAPI_EXT_ERRORS) then
    begin
        DoSimpleMsg(DSSPrime, 
            'The number of states provided (%d) does not match the number of phases (%d).', 
                [ValueCount, Integer(elem.ControlledElement.NPhases)],
            97896
        );
        Exit;
    end;
        
    if Count > elem.ControlledElement.NPhases then
        Count := elem.ControlledElement.NPhases;

    for i := 1 to Count Do 
    begin
        if Length(Value[i - 1]) > 0 then
            case AnsiLowerCase(Value[i - 1])[1] of
                'o': elem.FPresentState[i] := CTRL_OPEN;
                'c': elem.FPresentState[i] := CTRL_CLOSE;
            end;
    end;
    elem.PropertySideEffects(ord(TFuseProp.State));
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_NormalState(ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
var
    Value: PPAnsiCharArray0;
    i: Integer;
    Count: Integer;
    elem: TObj;
begin
    if (not _activeObj(DSSPrime, elem)) or (elem.ControlledElement = NIL) then
        Exit;

    Value := PPAnsiCharArray0(ValuePtr);

    Count := ValueCount;
    if (Count <> elem.ControlledElement.NPhases) AND (DSS_CAPI_EXT_ERRORS) then
    begin
        DoSimpleMsg(DSSPrime, 
            'The number of states provided (%d) does not match the number of phases (%d).', 
                [ValueCount, Integer(elem.ControlledElement.NPhases)],
            97897
        );
        Exit;
    end;
        
    if Count > elem.ControlledElement.NPhases then
        Count := elem.ControlledElement.NPhases;

    for i := 1 to Count Do 
    begin
        if Length(Value[i - 1]) > 0 then
            case AnsiLowerCase(Value[i - 1])[1] of
                'o': elem.FNormalState[i] := CTRL_OPEN;
                'c': elem.FNormalState[i] := CTRL_CLOSE;
            end;
    end;
    elem.NormalStateSet := True;
end;
//------------------------------------------------------------------------------
end.
