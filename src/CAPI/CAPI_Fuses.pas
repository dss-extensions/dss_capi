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

implementation

uses
    CAPI_Constants,
    Executive,
    Sysutils,
    Fuse,
    DSSPointerlist,
    DSSGlobals,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TFuseObj): Boolean; inline;
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
            DoSimpleMsg(DSS, 'No active Fuse object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const parm: String; const val: String);
var
    cmd: String;
    obj: TFuseObj;
begin
    if not _activeObj(DSS, obj) then
        Exit;
    
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Fuse.%s.%s=%s', [obj.Name, parm, val]);
    DSS.DSSExecutive.Command := cmd;
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
    elem: TFuseObj;
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
        DoSimpleMsg(DSSPrime, 'Fuse "' + Value + '" Not Found in Active Circuit.', 77003);
    end;
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.MonitoredElementName);
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.ElementName);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, 'monitoredObj', Value);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredTerm(Value: Integer); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, 'monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, 'SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedTerm(Value: Integer); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, 'SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Fuses_Get_TCCcurve(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        Result := DSS_GetAsPAnsiChar(DSSPrime, 'No Fuse Active!');
        Exit;
    end;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.FuseCurve.Name);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_TCCcurve(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, 'FuseCurve', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_RatedCurrent(): Double; CDECL;
var
    elem: TFuseObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.RatedCurrent
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_RatedCurrent(Value: Double); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, 'RatedCurrent', Format('%.8g ', [Value]));
end;
//------------------------------------------------------------------------------
function Fuses_Get_Delay(): Double; CDECL;
var
    elem: TFuseObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.DelayTime;
end;
//------------------------------------------------------------------------------
procedure Fuses_Open(); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.ControlledElement.Closed[0] := FALSE; // Open all phases
end;
//------------------------------------------------------------------------------
procedure Fuses_Close(); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.Reset();
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Delay(Value: Double); CDECL;
var
    elem: TFuseObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Set_Parameter(DSSPrime, 'Delay', Format('%.8g ', [Value]));
end;
//------------------------------------------------------------------------------
function Fuses_IsBlown(): TAPIBoolean; CDECL;
// Return TRUE if any phase blown
var
    elem: TFuseObj;
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
    pFuse: TFuseObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pFuse := DSSPrime.ActiveCircuit.Fuses.Get(Value);
    if pFuse = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Fuse index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pFuse;
end;
//------------------------------------------------------------------------------
function Fuses_Get_NumPhases(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.NPhases;
end;
//------------------------------------------------------------------------------
end.
