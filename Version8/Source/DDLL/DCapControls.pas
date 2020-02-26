unit DCapControls;

interface

function CapControlsI(mode: Longint; arg: Longint): Longint; CDECL;
function CapControlsF(mode: Longint; arg: Double): Double; CDECL;
function CapControlsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure CapControlsV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
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
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].CapControls.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('capcontrol.%s.%s=%s', [ActiveCapControl.Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function CapControlsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    elem: TCapControlObj;
    lst: TPointerList;

begin
    Result := 0;  // Default return value
    case mode of
        0:
        begin  // CapControls.First
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
        1:
        begin  // CapControls.Next
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
        2:
        begin  // CapControls.Mode read
            Result := 1;
            elem := ActiveCapControl;
            if elem <> NIL then
            begin
                case elem.CapControlType of
                    CURRENTCONTROL:
                        Result := 0;
                    VOLTAGECONTROL:
                        Result := 1;
                    KVARCONTROL:
                        Result := 2;
                    TIMECONTROL:
                        Result := 3;
                    PFCONTROL:
                        Result := 4;
                    USERCONTROL:
                        Result := 4;
                end;
            end;
        end;
        3:
        begin  // CapControls.Mode write
            elem := ActiveCapControl;
            if elem <> NIL then
            begin
                case arg of
                    0:
                        elem.CapControlType := CURRENTCONTROL;
                    1:
                        elem.CapControlType := VOLTAGECONTROL;
                    2:
                        elem.CapControlType := KVARCONTROL;
                    3:
                        elem.CapControlType := TIMECONTROL;
                    4:
                        elem.CapControlType := PFCONTROL;
                end;
            end;
        end;
        4:
        begin  // CapControls.MonitoredTerm read
            Result := 0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.ElementTerminal;
        end;
        5:
        begin  // CapControls.MonitoredTerm write
            Set_Parameter('Terminal', IntToStr(arg));
        end;
        6:
        begin  // CapControls.UseVoltOverride read
            Result := 0;
            elem := ActiveCapControl;
            if elem <> NIL then
                if elem.UseVoltageOverride then
                    Result := 1;
        end;
        7:
        begin  // CapControls.UseVoltOverride write
            if arg = 1 then
                Set_Parameter('VoltOverride', 'Yes')
            else
                Set_Parameter('VoltOverride', 'No');
        end;
        8:
        begin  // CapControls.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].CapControls.ListSize;
        end
    else
        Result := -1;
    end;
end;

//********************************Floating point type properties*******************
function CapControlsF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TCapControlObj;

begin
    Result := 0.0;
    case mode of
        0:
        begin  // CapControls.CTRatio read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.CTRatioVal;
        end;
        1:
        begin  // CapControls.CTRatio write
            Set_Parameter('CTratio', FloatToStr(arg));
        end;
        2:
        begin  // CapControls.PTRatio read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.PTRatioVal;
        end;
        3:
        begin  // CapControls.PTRatio write
            Set_Parameter('PTratio', FloatToStr(arg));
        end;
        4:
        begin  // CapControls.ONSetting read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.OnValue;
        end;
        5:
        begin  // CapControls.ONSetting write
            Set_Parameter('OnSetting', FloatToStr(arg));
        end;
        6:
        begin  // CapControls.OFFSetting read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.OffValue;
        end;
        7:
        begin  // CapControls.OFFSetting write
            Set_Parameter('OffSetting', FloatToStr(arg));
        end;
        8:
        begin  // CapControls.Vmax read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.VmaxVal;
        end;
        9:
        begin  // CapControls.Vmax write
            Set_Parameter('Vmax', FloatToStr(arg));
        end;
        10:
        begin  // CapControls.Vmin read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.VminVal;
        end;
        11:
        begin  // CapControls.Vmin write
            Set_Parameter('Vmin', FloatToStr(arg));
        end;
        12:
        begin  // CapControls.Delay read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.OnDelayVal;
        end;
        13:
        begin  // CapControls.Delay write
            Set_Parameter('Delay', FloatToStr(arg));
        end;
        14:
        begin  // CapControls.DelayOff read
            Result := 0.0;
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := elem.OffDelayVal;
        end;
        15:
        begin  // CapControls.DelayOff write
            Set_Parameter('DelayOff', FloatToStr(arg));
        end
    else
        Result := -1.0;
    end;
end;

//******************************String type properties****************************
function CapControlsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    elem: TCapControlObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    lst: TPointerList;

begin
    Result := pAnsiChar(Ansistring('0'));
    case mode of
        0:
        begin  // CapControl.Name read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.Name));
        end;
        1:
        begin  // CapControl.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].CapControls;
                S := Widestring(arg);  // Convert to Pascal String
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
        2:
        begin  // CapControl.Capacitor read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.This_Capacitor.Name));
        end;
        3:
        begin  // CapControl.Capacitor write
            Set_Parameter('Capacitor', Widestring(arg));
        end;
        4:
        begin  // CapControl.MonitoredObj read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveCapControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.ElementName));
        end;
        5:
        begin  // CapControl.MonitoredObj write
            Set_Parameter('Element', Widestring(arg));
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//******************************Variant type properties****************************
procedure CapControlsV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TCapControlObj;
    lst: TPointerList;
    k: Integer;

begin
    case mode of
        0:
        begin  // Capcontrols.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if CapControls.ListSize > 0 then
                    begin
                        lst := CapControls;
                        VarArrayRedim(arg, lst.ListSize - 1);
                        k := 0;
                        elem := lst.First;
                        while elem <> NIL do
                        begin
                            arg[k] := elem.Name;
                            Inc(k);
                            elem := lst.Next;
                        end;
                    end;
        end
    else
        arg[0] := 'Error, parameter not valid;'
    end;
end;

end.
