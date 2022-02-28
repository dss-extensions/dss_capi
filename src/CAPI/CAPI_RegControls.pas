unit CAPI_RegControls;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure RegControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure RegControls_Get_AllNames_GR(); CDECL;
function RegControls_Get_CTPrimary(): Double; CDECL;
function RegControls_Get_Delay(): Double; CDECL;
function RegControls_Get_First(): Integer; CDECL;
function RegControls_Get_ForwardBand(): Double; CDECL;
function RegControls_Get_ForwardR(): Double; CDECL;
function RegControls_Get_ForwardVreg(): Double; CDECL;
function RegControls_Get_ForwardX(): Double; CDECL;
function RegControls_Get_IsInverseTime(): TAPIBoolean; CDECL;
function RegControls_Get_IsReversible(): TAPIBoolean; CDECL;
function RegControls_Get_MaxTapChange(): Integer; CDECL;
function RegControls_Get_MonitoredBus(): PAnsiChar; CDECL;
function RegControls_Get_Name(): PAnsiChar; CDECL;
function RegControls_Get_Next(): Integer; CDECL;
function RegControls_Get_PTratio(): Double; CDECL;
function RegControls_Get_ReverseBand(): Double; CDECL;
function RegControls_Get_ReverseR(): Double; CDECL;
function RegControls_Get_ReverseVreg(): Double; CDECL;
function RegControls_Get_ReverseX(): Double; CDECL;
function RegControls_Get_TapDelay(): Double; CDECL;
function RegControls_Get_TapWinding(): Integer; CDECL;
function RegControls_Get_Transformer(): PAnsiChar; CDECL;
function RegControls_Get_VoltageLimit(): Double; CDECL;
function RegControls_Get_Winding(): Integer; CDECL;
function RegControls_Get_TapNumber(): Integer; CDECL;
procedure RegControls_Set_CTPrimary(Value: Double); CDECL;
procedure RegControls_Set_Delay(Value: Double); CDECL;
procedure RegControls_Set_ForwardBand(Value: Double); CDECL;
procedure RegControls_Set_ForwardR(Value: Double); CDECL;
procedure RegControls_Set_ForwardVreg(Value: Double); CDECL;
procedure RegControls_Set_ForwardX(Value: Double); CDECL;
procedure RegControls_Set_IsInverseTime(Value: TAPIBoolean); CDECL;
procedure RegControls_Set_IsReversible(Value: TAPIBoolean); CDECL;
procedure RegControls_Set_MaxTapChange(Value: Integer); CDECL;
procedure RegControls_Set_MonitoredBus(const Value: PAnsiChar); CDECL;
procedure RegControls_Set_Name(const Value: PAnsiChar); CDECL;
procedure RegControls_Set_PTratio(Value: Double); CDECL;
procedure RegControls_Set_ReverseBand(Value: Double); CDECL;
procedure RegControls_Set_ReverseR(Value: Double); CDECL;
procedure RegControls_Set_ReverseVreg(Value: Double); CDECL;
procedure RegControls_Set_ReverseX(Value: Double); CDECL;
procedure RegControls_Set_TapDelay(Value: Double); CDECL;
procedure RegControls_Set_TapWinding(Value: Integer); CDECL;
procedure RegControls_Set_Transformer(const Value: PAnsiChar); CDECL;
procedure RegControls_Set_VoltageLimit(Value: Double); CDECL;
procedure RegControls_Set_Winding(Value: Integer); CDECL;
procedure RegControls_Set_TapNumber(Value: Integer); CDECL;
function RegControls_Get_Count(): Integer; CDECL;
procedure RegControls_Reset(); CDECL;

// API extensions
function RegControls_Get_idx(): Integer; CDECL;
procedure RegControls_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    ControlElem,
    RegControl,
    SysUtils,
    DSSPointerList,
    DSSClass,
    DSSHelper,
    DSSObjectHelper;

type
    TObj = TRegControlObj;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.RegControls.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['RegControl'], 8989);
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
procedure RegControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.RegControls, False);
end;

procedure RegControls_Get_AllNames_GR(); CDECL;
// Same as RegControls_Get_AllNames but uses global result (GR) pointers
begin
    RegControls_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function RegControls_Get_CTPrimary(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.CTRating;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Delay(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TimeDelay;
end;
//------------------------------------------------------------------------------
function RegControls_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.RegControls);
end;
//------------------------------------------------------------------------------
function RegControls_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.RegControls);
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardBand(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Bandwidth;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardR(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.R;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardVreg(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Vreg;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardX(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.X;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsInverseTime(): TAPIBoolean; CDECL;
var
    elem: TObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.InverseTime;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsReversible(): TAPIBoolean; CDECL;
var
    elem: TObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.IsReversible;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MaxTapChange(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TapLimitPerChange;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MonitoredBus(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.RegulatedBus);
end;
//------------------------------------------------------------------------------
function RegControls_Get_Name(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
function RegControls_Get_PTratio(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PTRatio;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseBand(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.revBandwidth;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseR(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.revR;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseVreg(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.revVreg;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseX(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.revX;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapDelay(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TapDelay;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapWinding(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TrWinding;  // has the taps
end;
//------------------------------------------------------------------------------
function RegControls_Get_Transformer(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Transformer.Name);
end;
//------------------------------------------------------------------------------
function RegControls_Get_VoltageLimit(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Vlimit;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Winding(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.ElementTerminal;  // monitored winding
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapNumber(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TapNum;  // tap number on the controlled-winding of the transformer controlled by this regcontrol
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_CTPrimary(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.CTprim), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Delay(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Delay), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardBand(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Band), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardR(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.R), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardVreg(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Vreg), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardX(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.X), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsInverseTime(Value: TAPIBoolean); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.InverseTime), Integer(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsReversible(Value: TAPIBoolean); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Reversible), Integer(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MaxTapChange(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.MaxTapChange), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MonitoredBus(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Bus), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.RegControlClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.RegControlClass.ElementList.Active;
        DSSPrime.ActiveCircuit.RegControls.Get(DSSPrime.RegControlClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'RegControl "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_PTratio(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.PTratio), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseBand(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.RevBand), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseR(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.RevR), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseVreg(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.RevVreg), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseX(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.RevX), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapDelay(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.TapDelay), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapWinding(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.TapWinding), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Transformer(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Transformer), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_VoltageLimit(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Vlimit), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Winding(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.Winding), Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapNumber(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TRegControlProp.TapNum), Value);
end;
//------------------------------------------------------------------------------
function RegControls_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.RegControls.Count;
end;
//------------------------------------------------------------------------------
procedure RegControls_Reset(); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Reset();
end;
//------------------------------------------------------------------------------
function RegControls_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.RegControls.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_idx(Value: Integer); CDECL;
var
    pRegControl: TObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pRegControl := DSSPrime.ActiveCircuit.RegControls.Get(Value);
    if pRegControl = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['RegControl', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pRegControl;
end;
//------------------------------------------------------------------------------
end.
