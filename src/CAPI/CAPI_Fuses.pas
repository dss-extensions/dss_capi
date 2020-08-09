unit CAPI_Fuses;

interface

uses
    CAPI_Utils;

procedure Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
function Fuses_IsBlown(): Boolean; CDECL;
function Fuses_Get_idx(): Integer; CDECL;
procedure Fuses_Set_idx(Value: Integer); CDECL;
function Fuses_Get_NumPhases(): Integer; CDECL;

implementation

uses
    CAPI_Constants,
    Executive,
    Sysutils,
    Fuse,
    Pointerlist,
    DSSGlobals;

//------------------------------------------------------------------------------
function _activeObj(out obj: TFuseObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.Fuses.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active Fuse object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    obj: TFuseObj;
begin
    if not _activeObj(obj) then
        Exit;
    
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Fuse.%s.%s=%s', [obj.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Fuses, False);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Fuses.ListSize;
end;
//------------------------------------------------------------------------------
function Fuses_Get_First(): Integer; CDECL;
var
    pElem: TFuseObj;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    pElem := ActiveCircuit.Fuses.First;
    if pElem = NIL then
        Exit;

    repeat
        if pElem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pElem;
            Result := 1;
        end
        else
            pElem := ActiveCircuit.Fuses.Next;
    until (Result = 1) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Name(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Next(): Integer; CDECL;
var
    pElem: TFuseObj;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    pElem := ActiveCircuit.Fuses.Next;
    if pElem = NIL then
        Exit;
        
    repeat
        if pElem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pElem;
            Result := ActiveCircuit.Fuses.ActiveIndex;
        end
        else
            pElem := ActiveCircuit.Fuses.Next;
    until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit then
        Exit;
    if FuseClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := FuseClass.ElementList.Active;
        ActiveCircuit.Fuses.Get(FuseClass.Active);
    end
    else
    begin
        DoSimpleMsg('Fuse "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.MonitoredElementName);
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredTerm(Value: Integer); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    Set_parameter('monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    Set_parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedTerm(Value: Integer); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Fuses_Get_TCCcurve(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
    begin
        Result := DSS_GetAsPAnsiChar('No Fuse Active!');
        Exit;
    end;

    Result := DSS_GetAsPAnsiChar(elem.FuseCurve.Name);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_TCCcurve(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    Set_parameter('FuseCurve', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_RatedCurrent(): Double; CDECL;
var
    elem: TFuseObj;
begin
    Result := -1.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.RatedCurrent
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_RatedCurrent(Value: Double); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    Set_parameter('RatedCurrent', Format('%.8g ', [Value]));
end;
//------------------------------------------------------------------------------
function Fuses_Get_Delay(): Double; CDECL;
var
    elem: TFuseObj;
begin
    Result := -1.0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.DelayTime;
end;
//------------------------------------------------------------------------------
procedure Fuses_Open(); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.ControlledElement.Closed[0] := FALSE; // Open all phases
end;
//------------------------------------------------------------------------------
procedure Fuses_Close(); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    elem.Reset();
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Delay(Value: Double); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(elem) then
        Exit;

    Set_parameter('Delay', Format('%.8g ', [Value]));
end;
//------------------------------------------------------------------------------
function Fuses_IsBlown(): Boolean; CDECL;
// Return TRUE if any phase blown
var
    elem: TFuseObj;
    i: Integer;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;

    for i := 1 to elem.nphases do
        if not elem.ControlledElement.Closed[i] then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function Fuses_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.Fuses.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_idx(Value: Integer); CDECL;
var
    pFuse: TFuseObj;
begin
    if InvalidCircuit then
        Exit;
    pFuse := ActiveCircuit.Fuses.Get(Value);
    if pFuse = NIL then
    begin
        DoSimpleMsg('Invalid Fuse index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pFuse;
end;
//------------------------------------------------------------------------------
function Fuses_Get_NumPhases(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;

    Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
end.
