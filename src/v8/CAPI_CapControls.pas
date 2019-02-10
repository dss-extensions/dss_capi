unit CAPI_CapControls;

{$inline on}

interface

uses
    CAPI_Utils;

procedure CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
function CapControls_Get_UseVoltOverride(): Wordbool; CDECL;
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
procedure CapControls_Set_UseVoltOverride(Value: Wordbool); CDECL;
procedure CapControls_Set_Vmax(Value: Double); CDECL;
procedure CapControls_Set_Vmin(Value: Double); CDECL;
function CapControls_Get_Count(): Integer; CDECL;
procedure CapControls_Reset(); CDECL;

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

function ActiveCapControl: TCapControlObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].CapControls.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capcontrol.%s.%s=%s', [ActiveCapControl.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure CapControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    elem: TCapControlObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if CapControls.ListSize > 0 then
            begin
                lst := CapControls;
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (lst.ListSize - 1) + 1);
                k := 0;
                elem := lst.First;
                while elem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(elem.Name);
                    Inc(k);
                    elem := lst.Next;
                end;
            end;
end;

procedure CapControls_Get_AllNames_GR(); CDECL;
// Same as CapControls_Get_AllNames but uses global result (GR) pointers
begin
    CapControls_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function CapControls_Get_Capacitor_AnsiString(): Ansistring; inline;
var
    elem: TCapControlObj;
begin
    Result := '';
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.This_Capacitor.Name;
end;

function CapControls_Get_Capacitor(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CapControls_Get_Capacitor_AnsiString());
end;
//------------------------------------------------------------------------------
function CapControls_Get_CTratio(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.CTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DeadTime(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.DeadTimeVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Delay(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OnDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_DelayOff(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OffDelayVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_First(): Integer; CDECL;
var
    elem: TCapControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].CapControls;
        elem := lst.First;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := 1;
                end
                else
                    elem := lst.Next;
            until (Result = 1) or (elem = NIL);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Mode(): Integer; CDECL;
var
    elem: TCapControlObj;
begin
    Result := dssCapControlVoltage;
    elem := ActiveCapControl;
    if elem <> NIL then
    begin
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
end;
//------------------------------------------------------------------------------
function CapControls_Get_MonitoredObj_AnsiString(): Ansistring; inline;
var
    elem: TCapControlObj;
begin
    Result := '';
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.ElementName;
end;

function CapControls_Get_MonitoredObj(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CapControls_Get_MonitoredObj_AnsiString());
end;
//------------------------------------------------------------------------------
function CapControls_Get_MonitoredTerm(): Integer; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TCapControlObj;
begin
    Result := '';
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.Name;
end;

function CapControls_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CapControls_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function CapControls_Get_Next(): Integer; CDECL;
var
    elem: TCapControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].CapControls;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;
//------------------------------------------------------------------------------
function CapControls_Get_OFFSetting(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OffValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_ONSetting(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OnValue;
end;
//------------------------------------------------------------------------------
function CapControls_Get_PTratio(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.PTRatioVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_UseVoltOverride(): Wordbool; CDECL;
var
    elem: TCapControlObj;
begin
    Result := FALSE;
    elem := ActiveCapControl;
    if elem <> NIL then
        if elem.UseVoltageOverride then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmax(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.VmaxVal;
end;
//------------------------------------------------------------------------------
function CapControls_Get_Vmin(): Double; CDECL;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
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
    elem := ActiveCapControl;
    if elem <> NIL then
    begin
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
var
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    elem: TCapControlObj;
    lst: TPointerList;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        lst := ActiveCircuit[ActiveActor].CapControls;
        S := Value;  // Convert to Pascal String
        Found := FALSE;
        ActiveSave := lst.ActiveIndex;
        elem := lst.First;
        while elem <> NIL do
        begin
            if (CompareText(elem.Name, S) = 0) then
            begin
                ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                Found := TRUE;
                Break;
            end;
            elem := lst.Next;
        end;
        if not Found then
        begin
            DoSimpleMsg('CapControl "' + S + '" Not Found in Active Circuit.', 5003);
            elem := lst.Get(ActiveSave);    // Restore active Load
            ActiveCircuit[ActiveActor].ActiveCktElement := elem;
        end;
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
procedure CapControls_Set_UseVoltOverride(Value: Wordbool); CDECL;
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
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].CapControls.ListSize;
end;
//------------------------------------------------------------------------------
procedure CapControls_Reset(); CDECL;
var
    elem: TCapControlObj;
begin
    elem := ActiveCapControl;
    if elem <> NIL then
    begin
        elem.Reset;
    end;

end;
//------------------------------------------------------------------------------
end.
