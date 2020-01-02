unit CAPI_Fuses;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
    DSSGlobals,
    DSSClass,
    DSSHelper;
   

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(DSSPrime.ActiveCircuit) then
        exit;
    DSSPrime.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('Fuse.%s.%s=%s', [TFuseObj(DSSPrime.ActiveCircuit.Fuses.Active).Name, parm, val]);
    DSSPrime.DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Fuses_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Fuses, False);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Fuses.ListSize;
end;
//------------------------------------------------------------------------------
function Fuses_Get_First(): Integer; CDECL;
var
    pElem: TFuseObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    pElem := DSSPrime.ActiveCircuit.Fuses.First;
    if pElem <> NIL then
        repeat
            if pElem.Enabled then
            begin
                DSSPrime.ActiveCircuit.ActiveCktElement := pElem;
                Result := 1;
            end
            else
                pElem := DSSPrime.ActiveCircuit.Fuses.Next;
        until (Result = 1) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Name(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function Fuses_Get_Next(): Integer; CDECL;
var
    pElem: TFuseObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    pElem := DSSPrime.ActiveCircuit.Fuses.Next;
    if pElem <> NIL then
        repeat
            if pElem.Enabled then
            begin
                DSSPrime.ActiveCircuit.ActiveCktElement := pElem;
                Result := DSSPrime.ActiveCircuit.Fuses.ActiveIndex;
            end
            else
                pElem := DSSPrime.ActiveCircuit.Fuses.Next;
        until (Result > 0) or (pElem = NIL);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if DSSPrime.ActiveCircuit = NIL then
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
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.MonitoredElementName);
end;
//------------------------------------------------------------------------------
function Fuses_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := elem.MonitoredElementTerminal;
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedObj(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Set_parameter('monitoredObj', Value);
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_MonitoredTerm(Value: Integer); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Set_parameter('monitoredterm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedObj(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Set_parameter('SwitchedObj', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_SwitchedTerm(): Integer; CDECL;
var
    elem: TFuseObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_SwitchedTerm(Value: Integer); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Set_parameter('SwitchedTerm', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function Fuses_Get_TCCcurve(): PAnsiChar; CDECL;
var
    elem: TFuseObj;
begin
    Result := nil;
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := DSS_GetAsPAnsiChar(elem.FuseCurve.Name)
    else
        Result := DSS_GetAsPAnsiChar('No Fuse Active!');
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_TCCcurve(const Value: PAnsiChar); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Set_parameter('FuseCurve', Value);
end;
//------------------------------------------------------------------------------
function Fuses_Get_RatedCurrent(): Double; CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := elem.RatedCurrent
    else
        Result := -1.0;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_RatedCurrent(Value: Double); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Set_parameter('RatedCurrent', Format('%.8g ', [Value]));
end;
//------------------------------------------------------------------------------
function Fuses_Get_Delay(): Double; CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        Result := elem.DelayTime
    else
        Result := -1.0;
end;
//------------------------------------------------------------------------------
procedure Fuses_Open(); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        elem.ControlledElement.Closed[0] := FALSE; // Open all phases
end;
//------------------------------------------------------------------------------
procedure Fuses_Close(); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
        elem.Reset;
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_Delay(Value: Double); CDECL;
var
    elem: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
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
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    elem := DSSPrime.ActiveCircuit.Fuses.Active;
    if elem <> NIL then
    begin
        for i := 1 to elem.nphases do
            if not elem.ControlledElement.Closed[i] then
                Result := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function Fuses_Get_idx(): Integer; CDECL;
begin
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.Fuses.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure Fuses_Set_idx(Value: Integer); CDECL;
var
    pFuse: TFuseObj;
begin
    if DSSPrime.ActiveCircuit = NIL then
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
    pFuse: TFuseObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pFuse := DSSPrime.ActiveCircuit.Fuses.Active;
        if pFuse <> NIL then
            Result := pFuse.NPhases;
    end;
end;
//------------------------------------------------------------------------------
end.
