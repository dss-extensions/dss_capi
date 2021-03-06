unit CAPI_CapControls;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure CapControls_Get_AllNames_GR(); CDECL;
function CapControls_Get_Capacitor(): PAnsiChar; CDECL;
function CapControls_Get_CTratio(): Double; CDECL;
function CapControls_Get_DeadTime(): Double; CDECL;
function CapControls_Get_Delay(): Double; CDECL;
function CapControls_Get_DelayOff(): Double; CDECL;
function CapControls_Get_First(): Integer; CDECL;
function CapControls_Get_Mode(): Integer; CDECL;
function CapControls_Get_MonitoredObj(): PAnsiChar; CDECL;
function CapControls_Get_MonitoredTerm(): Integer; CDECL;
function CapControls_Get_Name(): PAnsiChar; CDECL;
function CapControls_Get_Next(): Integer; CDECL;
function CapControls_Get_OFFSetting(): Double; CDECL;
function CapControls_Get_ONSetting(): Double; CDECL;
function CapControls_Get_PTratio(): Double; CDECL;
function CapControls_Get_UseVoltOverride(): TAPIBoolean; CDECL;
function CapControls_Get_Vmax(): Double; CDECL;
function CapControls_Get_Vmin(): Double; CDECL;
procedure CapControls_Set_Capacitor(const Value: PAnsiChar); CDECL;
procedure CapControls_Set_CTratio(Value: Double); CDECL;
procedure CapControls_Set_DeadTime(Value: Double); CDECL;
procedure CapControls_Set_Delay(Value: Double); CDECL;
procedure CapControls_Set_DelayOff(Value: Double); CDECL;
procedure CapControls_Set_Mode(Value: Integer); CDECL;
procedure CapControls_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
procedure CapControls_Set_MonitoredTerm(Value: Integer); CDECL;
procedure CapControls_Set_Name(const Value: PAnsiChar); CDECL;
procedure CapControls_Set_OFFSetting(Value: Double); CDECL;
procedure CapControls_Set_ONSetting(Value: Double); CDECL;
procedure CapControls_Set_PTratio(Value: Double); CDECL;
procedure CapControls_Set_UseVoltOverride(Value: TAPIBoolean); CDECL;
procedure CapControls_Set_Vmax(Value: Double); CDECL;
procedure CapControls_Set_Vmin(Value: Double); CDECL;
function CapControls_Get_Count(): Integer; CDECL;
procedure CapControls_Reset(); CDECL;

// API Extensions
function CapControls_Get_idx(): Integer; CDECL;
procedure CapControls_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    ControlElem,
    CapControl,
    CapControlVars,
    SysUtils,
    DSSPointerList,
    Utilities,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TCapControlObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.CapControls.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active CapControl object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const parm: String; const val: String);
var
    cmd: String;
    elem: TCapControlObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capcontrol.%s.%s=%s', [elem.Name, parm, val]);
    DSS.DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.CapControls, False);
end;

procedure CapControls_Get_AllNames_GR(); CDECL;
// Same as CapControls_Get_AllNames but uses global result (GR) pointers
begin
    CapControls_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function CapControls_Get_Capacitor(): PAnsiChar; CDECL;
var
    elem: TCapControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.This_Capacitor.Name);
end;
//------------------------------------------------------------------------------
function CapControls_Get_CTratio(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.CTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DeadTime(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.DeadTimeVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Delay(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.OnDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DelayOff(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.OffDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.CapControls);
end;
//------------------------------------------------------------------------------
function CapControls_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.CapControls);
end;
//------------------------------------------------------------------------------
function CapControls_Get_Mode(): Integer; CDECL;
var
    elem: TCapControlObj;
begin
    Result := dssCapControlVoltage;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    case elem.CapControlType of
        CURRENTCONTROL:
            Result := dssCapControlCurrent;
        VOLTAGECONTROL:
            Result := dssCapControlVoltage;
        KVARCONTROL:
            Result := dssCapControlKvar;
        TIMECONTROL:
            Result := dssCapControlTime;
        PFCONTROL:
            Result := dssCapControlPF;
        USERCONTROL:
            Result := dssCapControlPF;
    end;
end;
//------------------------------------------------------------------------------
function CapControls_Get_MonitoredObj(): PAnsiChar; CDECL;
var
    elem: TCapControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.ElementName);
end;
//------------------------------------------------------------------------------
function CapControls_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Name(): PAnsiChar; CDECL;
var
    elem: TCapControlObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
function CapControls_Get_OFFSetting(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.OffValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_ONSetting(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.OnValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_PTratio(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_UseVoltOverride(): TAPIBoolean; CDECL;
var
    elem: TCapControlObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.UseVoltageOverride;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmax(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.VmaxVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmin(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.VminVal;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Capacitor(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, 'Capacitor', value);
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_CTratio(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'CTratio', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_DeadTime(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'DeadTime', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Delay(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'Delay', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_DelayOff(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'DelayOff', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Mode(Value: Integer); CDECL;
var
    elem: TCapControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    case Value of
        dssCapControlCurrent:
            elem.CapControlType := CURRENTCONTROL;
        dssCapControlVoltage:
            elem.CapControlType := VOLTAGECONTROL;
        dssCapControlKvar:
            elem.CapControlType := KVARCONTROL;
        dssCapControlTime:
            elem.CapControlType := TIMECONTROL;
        dssCapControlPF:
            elem.CapControlType := PFCONTROL;
    end;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_MonitoredObj(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, 'Element', value);
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_MonitoredTerm(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, 'Terminal', IntToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.CapControlClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.CapControlClass.ElementList.Active;
        DSSPrime.ActiveCircuit.CapControls.Get(DSSPrime.CapControlClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'CapControl "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_OFFSetting(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'OffSetting', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_ONSetting(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'OnSetting', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_PTratio(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'PTratio', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_UseVoltOverride(Value: TAPIBoolean); CDECL;
begin
    Set_Parameter(DSSPrime, 'VoltOverride', StrYorN(Value = True))
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Vmax(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'Vmax', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Vmin(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, 'Vmin', FloatToStr(value));
end;
//------------------------------------------------------------------------------
function CapControls_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(DSSPrime.ActiveCircuit) then
        Result := DSSPrime.ActiveCircuit.CapControls.Count;
end;
//------------------------------------------------------------------------------
procedure CapControls_Reset(); CDECL;
var
    elem: TCapControlObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Reset();
end;
//------------------------------------------------------------------------------
function CapControls_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.CapControls.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_idx(Value: Integer); CDECL;
var
    pCapControl: TCapControlObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pCapControl := DSSPrime.ActiveCircuit.CapControls.Get(Value);
    if pCapControl = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid CapControl index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pCapControl;
end;
//------------------------------------------------------------------------------
end.
