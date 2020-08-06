unit CAPI_CapControls;

interface

uses
    CAPI_Utils;

procedure CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
function CapControls_Get_UseVoltOverride(): Boolean; CDECL;
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
procedure CapControls_Set_UseVoltOverride(Value: Boolean); CDECL;
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
    PointerList;

//------------------------------------------------------------------------------
function _activeObj(out obj: TCapControlObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.CapControls.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active CapControl object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
    elem: TCapControlObj;
begin
    if not _activeObj(elem) then
        Exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capcontrol.%s.%s=%s', [elem.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.CapControls, False);
end;
//------------------------------------------------------------------------------
function CapControls_Get_Capacitor(): PAnsiChar; CDECL;
var
    elem: TCapControlObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.This_Capacitor.Name);
end;
//------------------------------------------------------------------------------
function CapControls_Get_CTratio(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.CTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DeadTime(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.DeadTimeVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Delay(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.OnDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DelayOff(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.OffDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_First(): Integer; CDECL;
var
    elem: TCapControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    
    lst := ActiveCircuit.CapControls;
    elem := lst.First;
    if elem = NIL then
        Exit;
        
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
//------------------------------------------------------------------------------
function CapControls_Get_Mode(): Integer; CDECL;
var
    elem: TCapControlObj;
begin
    Result := dssCapControlVoltage;
    if not _activeObj(elem) then
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
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.ElementName);
end;
//------------------------------------------------------------------------------
function CapControls_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Name(): PAnsiChar; CDECL;
var
    elem: TCapControlObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
function CapControls_Get_Next(): Integer; CDECL;
var
    elem: TCapControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    
    lst := ActiveCircuit.CapControls;
    elem := lst.Next;
    if elem = NIL then
        Exit;
        
    repeat
        if elem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := elem;
            Result := lst.ActiveIndex;
        end
        else
            elem := lst.Next;
    until (Result > 0) or (elem = NIL);
end;
//------------------------------------------------------------------------------
function CapControls_Get_OFFSetting(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.OffValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_ONSetting(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.OnValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_PTratio(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.PTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_UseVoltOverride(): Boolean; CDECL;
var
    elem: TCapControlObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := elem.UseVoltageOverride;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmax(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.VmaxVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmin(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.VminVal;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Capacitor(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('Capacitor', value);
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_CTratio(Value: Double); CDECL;
begin
    Set_Parameter('CTratio', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_DeadTime(Value: Double); CDECL;
begin
    Set_Parameter('DeadTime', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Delay(Value: Double); CDECL;
begin
    Set_Parameter('Delay', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_DelayOff(Value: Double); CDECL;
begin
    Set_Parameter('DelayOff', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Mode(Value: Integer); CDECL;
var
    elem: TCapControlObj;
begin
    if not _activeObj(elem) then
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
    Set_Parameter('Element', value);
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_MonitoredTerm(Value: Integer); CDECL;
begin
    Set_Parameter('Terminal', IntToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;

    if CapControlClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := CapControlClass.ElementList.Active;
        ActiveCircuit.CapControls.Get(CapControlClass.Active);
    end
    else
    begin
        DoSimpleMsg('CapControl "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_OFFSetting(Value: Double); CDECL;
begin
    Set_Parameter('OffSetting', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_ONSetting(Value: Double); CDECL;
begin
    Set_Parameter('OnSetting', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_PTratio(Value: Double); CDECL;
begin
    Set_Parameter('PTratio', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_UseVoltOverride(Value: Boolean); CDECL;
begin
    if Value = TRUE then
        Set_Parameter('VoltOverride', 'Yes')
    else
        Set_Parameter('VoltOverride', 'No');
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Vmax(Value: Double); CDECL;
begin
    Set_Parameter('Vmax', FloatToStr(value));
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_Vmin(Value: Double); CDECL;
begin
    Set_Parameter('Vmin', FloatToStr(value));
end;
//------------------------------------------------------------------------------
function CapControls_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.CapControls.ListSize;
end;
//------------------------------------------------------------------------------
procedure CapControls_Reset(); CDECL;
var
    elem: TCapControlObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Reset();
end;
//------------------------------------------------------------------------------
function CapControls_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.CapControls.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure CapControls_Set_idx(Value: Integer); CDECL;
var
    pCapControl: TCapControlObj;
begin
    if InvalidCircuit then
        Exit;
    pCapControl := ActiveCircuit.CapControls.Get(Value);
    if pCapControl = NIL then
    begin
        DoSimpleMsg('Invalid CapControl index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pCapControl;
end;
//------------------------------------------------------------------------------
end.
