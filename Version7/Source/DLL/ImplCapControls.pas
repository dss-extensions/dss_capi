unit ImplCapControls;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TCapControls = class(TAutoObject, ICapControls)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Capacitor: Widestring; SAFECALL;
        function Get_CTratio: Double; SAFECALL;
        function Get_DeadTime: Double; SAFECALL;
        function Get_Delay: Double; SAFECALL;
        function Get_DelayOff: Double; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Mode: CapControlModes; SAFECALL;
        function Get_MonitoredObj: Widestring; SAFECALL;
        function Get_MonitoredTerm: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_OFFSetting: Double; SAFECALL;
        function Get_ONSetting: Double; SAFECALL;
        function Get_PTratio: Double; SAFECALL;
        function Get_UseVoltOverride: Wordbool; SAFECALL;
        function Get_Vmax: Double; SAFECALL;
        function Get_Vmin: Double; SAFECALL;
        procedure Set_Capacitor(const Value: Widestring); SAFECALL;
        procedure Set_CTratio(Value: Double); SAFECALL;
        procedure Set_DeadTime(Value: Double); SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        procedure Set_DelayOff(Value: Double); SAFECALL;
        procedure Set_Mode(Value: CapControlModes); SAFECALL;
        procedure Set_MonitoredObj(const Value: Widestring); SAFECALL;
        procedure Set_MonitoredTerm(Value: Integer); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        procedure Set_OFFSetting(Value: Double); SAFECALL;
        procedure Set_ONSetting(Value: Double); SAFECALL;
        procedure Set_PTratio(Value: Double); SAFECALL;
        procedure Set_UseVoltOverride(Value: Wordbool); SAFECALL;
        procedure Set_Vmax(Value: Double); SAFECALL;
        procedure Set_Vmin(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        procedure Reset; SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    ControlElem,
    CapControl,
    CapControlVars,
    Variants,
    SysUtils,
    PointerList;

function ActiveCapControl: TCapControlObj;
begin
    Result := NIL;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.CapControls.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capcontrol.%s.%s=%s', [ActiveCapControl.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TCapControls.Get_AllNames: Olevariant;
var
    elem: TCapControlObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if CapControls.ListSize > 0 then
            begin
                lst := CapControls;
                VarArrayRedim(Result, lst.ListSize - 1);
                k := 0;
                elem := lst.First;
                while elem <> NIL do
                begin
                    Result[k] := elem.Name;
                    Inc(k);
                    elem := lst.Next;
                end;
            end;
end;

function TCapControls.Get_Capacitor: Widestring;
var
    elem: TCapControlObj;
begin
    Result := '';
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.This_Capacitor.Name;
end;

function TCapControls.Get_CTratio: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.CTRatioVal;
end;

function TCapControls.Get_DeadTime: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.DeadTimeVal;
end;

function TCapControls.Get_Delay: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OnDelayVal;
end;

function TCapControls.Get_DelayOff: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OffDelayVal;
end;

function TCapControls.Get_First: Integer;
var
    elem: TCapControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.CapControls;
        elem := lst.First;
        if elem <> NIL then
        begin
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
    end;
end;

function TCapControls.Get_Mode: CapControlModes;
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

function TCapControls.Get_MonitoredObj: Widestring;
var
    elem: TCapControlObj;
begin
    Result := '';
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.ElementName;
end;

function TCapControls.Get_MonitoredTerm: Integer;
var
    elem: TCapControlObj;
begin
    Result := 0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.ElementTerminal;
end;

function TCapControls.Get_Name: Widestring;
var
    elem: TCapControlObj;
begin
    Result := '';
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.Name;
end;

function TCapControls.Get_Next: Integer;
var
    elem: TCapControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.CapControls;
        elem := lst.Next;
        if elem <> NIL then
        begin
            repeat
                if elem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := elem;
                    Result := lst.ActiveIndex;
                end
                else
                    elem := lst.Next;
            until (Result > 0) or (elem = NIL);
        end
    end;
end;

function TCapControls.Get_OFFSetting: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OffValue;
end;

function TCapControls.Get_ONSetting: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.OnValue;
end;

function TCapControls.Get_PTratio: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.PTRatioVal;
end;

function TCapControls.Get_UseVoltOverride: Wordbool;
var
    elem: TCapControlObj;
begin
    Result := FALSE;
    elem := ActiveCapControl;
    if elem <> NIL then
        if elem.UseVoltageOverride then
            Result := TRUE;
end;

function TCapControls.Get_Vmax: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.VmaxVal;
end;

function TCapControls.Get_Vmin: Double;
var
    elem: TCapControlObj;
begin
    Result := 0.0;
    elem := ActiveCapControl;
    if elem <> NIL then
        Result := elem.VminVal;
end;

procedure TCapControls.Set_Capacitor(const Value: Widestring);
begin
    Set_Parameter('Capacitor', value);
end;

procedure TCapControls.Set_CTratio(Value: Double);
begin
    Set_Parameter('CTratio', FloatToStr(value));
end;

procedure TCapControls.Set_DeadTime(Value: Double);
begin
    Set_Parameter('DeadTime', FloatToStr(value));
end;

procedure TCapControls.Set_Delay(Value: Double);
begin
    Set_Parameter('Delay', FloatToStr(value));
end;

procedure TCapControls.Set_DelayOff(Value: Double);
begin
    Set_Parameter('DelayOff', FloatToStr(value));
end;

procedure TCapControls.Set_Mode(Value: CapControlModes);
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

procedure TCapControls.Set_MonitoredObj(const Value: Widestring);
begin
    Set_Parameter('Element', value);
end;

procedure TCapControls.Set_MonitoredTerm(Value: Integer);
begin
    Set_Parameter('Terminal', IntToStr(value));
end;

procedure TCapControls.Set_Name(const Value: Widestring);
var
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    elem: TCapControlObj;
    lst: TPointerList;
begin
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.CapControls;
        S := Value;  // Convert to Pascal String
        Found := FALSE;
        ActiveSave := lst.ActiveIndex;
        elem := lst.First;
        while elem <> NIL do
        begin
            if (CompareText(elem.Name, S) = 0) then
            begin
                ActiveCircuit.ActiveCktElement := elem;
                Found := TRUE;
                Break;
            end;
            elem := lst.Next;
        end;
        if not Found then
        begin
            DoSimpleMsg('CapControl "' + S + '" Not Found in Active Circuit.', 5003);
            elem := lst.Get(ActiveSave);    // Restore active Load
            ActiveCircuit.ActiveCktElement := elem;
        end;
    end;
end;

procedure TCapControls.Set_OFFSetting(Value: Double);
begin
    Set_Parameter('OffSetting', FloatToStr(value));
end;

procedure TCapControls.Set_ONSetting(Value: Double);
begin
    Set_Parameter('OnSetting', FloatToStr(value));
end;

procedure TCapControls.Set_PTratio(Value: Double);
begin
    Set_Parameter('PTratio', FloatToStr(value));
end;

procedure TCapControls.Set_UseVoltOverride(Value: Wordbool);
begin
    if Value = TRUE then
        Set_Parameter('VoltOverride', 'Yes')
    else
        Set_Parameter('VoltOverride', 'No');
end;

procedure TCapControls.Set_Vmax(Value: Double);
begin
    Set_Parameter('Vmax', FloatToStr(value));
end;

procedure TCapControls.Set_Vmin(Value: Double);
begin
    Set_Parameter('Vmin', FloatToStr(value));
end;

function TCapControls.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.CapControls.ListSize;
end;

procedure TCapControls.Reset;
var
    elem: TCapControlObj;
begin
    elem := ActiveCapControl;
    if elem <> NIL then
    begin
        elem.Reset;
    end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TCapControls, Class_CapControls,
        ciInternal, tmApartment);
end.
