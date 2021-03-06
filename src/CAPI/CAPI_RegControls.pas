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
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TRegControlObj): Boolean; inline;
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
            DoSimpleMsg(DSS, 'No active RegControl object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const parm: String; const val: String);
var
    cmd: String;
    elem: TRegControlObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('regcontrol.%s.%s=%s', [elem.Name, parm, val]);
    DSS.DSSExecutive.Command := cmd;
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
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.CT;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Delay(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.InitialDelay;
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
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.BandVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardR(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.LineDropR;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardVreg(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TargetVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardX(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.LineDropX;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsInverseTime(): TAPIBoolean; CDECL;
var
    elem: TRegControlObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.IsInverseTime;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsReversible(): TAPIBoolean; CDECL;
var
    elem: TRegControlObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.UseReverseDrop;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MaxTapChange(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MaxTapChange;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MonitoredBus(): PAnsiChar; CDECL;
var
    elem: TRegControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.ControlledBusName);
end;
//------------------------------------------------------------------------------
function RegControls_Get_Name(): PAnsiChar; CDECL;
var
    elem: TRegControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
function RegControls_Get_PTratio(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PT;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseBand(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RevBandVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseR(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RevLineDropR;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseVreg(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RevTargetVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseX(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RevLineDropX;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapDelay(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.SubsequentDelay;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapWinding(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TrWinding;  // has the taps
end;
//------------------------------------------------------------------------------
function RegControls_Get_Transformer(): PAnsiChar; CDECL;
var
    elem: TRegControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Transformer.Name);
end;
//------------------------------------------------------------------------------
function RegControls_Get_VoltageLimit(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.VoltageLimit;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Winding(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.ElementTerminal;  // monitored winding
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapNumber(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TapNum;  // tap number on the controlled-winding of the transformer controlled by this regcontrol
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_CTPrimary(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'CTprim', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Delay(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'Delay', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardBand(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'Band', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardR(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'R', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardVreg(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'Vreg', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardX(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'X', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsInverseTime(Value: TAPIBoolean); CDECL;
begin
    if Value = TRUE then
        Set_Parameter(DSSPrime, 'InverseTime', 'y')
    else
        Set_Parameter(DSSPrime, 'InverseTime', 'n');
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsReversible(Value: TAPIBoolean); CDECL;
begin
    if Value = TRUE then
        Set_Parameter(DSSPrime, 'Reversible', 'y')
    else
        Set_Parameter(DSSPrime, 'Reversible', 'n');
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MaxTapChange(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, 'MaxTapChange', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MonitoredBus(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, 'Bus', Value);
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
        DoSimpleMsg(DSSPrime, 'RegControl "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_PTratio(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'PTratio', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseBand(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'RevBand', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseR(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'RevR', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseVreg(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'RevVreg', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseX(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'RevX', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapDelay(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'TapDelay', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapWinding(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, 'TapWinding', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Transformer(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, 'Transformer', Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_VoltageLimit(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'Vlimit', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Winding(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, 'Winding', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapNumber(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, 'TapNum', IntToStr(Value));
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
    elem: TRegControlObj;
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
    pRegControl: TRegControlObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pRegControl := DSSPrime.ActiveCircuit.RegControls.Get(Value);
    if pRegControl = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid RegControl index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pRegControl;
end;
//------------------------------------------------------------------------------
end.
