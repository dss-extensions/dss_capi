unit DRegControls;

interface

function RegControlsI(mode: Longint; arg: Longint): Longint; CDECL;
function RegControlsF(mode: Longint; arg: Double): Double; CDECL;
function RegControlsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure RegControlsV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    ControlElem,
    RegControl,
    Variants,
    SysUtils,
    PointerList;

function ActiveRegControl: TRegControlObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].RegControls.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('regcontrol.%s.%s=%s', [ActiveRegControl.Name, parm, val]);
    DSSExecutive[ActiveActor].Command := cmd;
end;

function RegControlsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    elem: TRegControlObj;
    lst: TPointerList;

begin
    Result := 0;          // Default return value
    case mode of
        0:
        begin  // RegControls.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].RegControls;
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
        begin  // RegControls.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].RegControls;
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
        begin  // RegControls.TapWinding read
            Result := 0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.TrWinding;  // has the taps
        end;
        3:
        begin  // RegControls.TapWinding write
            Set_Parameter('TapWinding', IntToStr(arg));
        end;
        4:
        begin  // RegControls.Winding read
            Result := 0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.ElementTerminal;  // monitored winding
        end;
        5:
        begin  // RegControls.Winding write
            Set_Parameter('Winding', IntToStr(arg));
        end;
        6:
        begin  // RegControls.IsReversible read
            Result := 0;
            elem := ActiveRegControl;
            if elem <> NIL then
                if elem.UseReverseDrop then
                    Result := 1;
        end;
        7:
        begin  // RegControls.IsReversible write
            if arg = 1 then
                Set_Parameter('Reversible', 'y')
            else
                Set_Parameter('Reversible', 'n');
        end;
        8:
        begin  // RegControls.IsInverseTime read
            Result := 0;
            elem := ActiveRegControl;
            if elem <> NIL then
                if elem.IsInverseTime then
                    Result := 1;
        end;
        9:
        begin  // RegControls.IsInverseTime write
            if arg = 1 then
                Set_Parameter('InverseTime', 'y')
            else
                Set_Parameter('InverseTime', 'n');
        end;
        10:
        begin  // RegControls.MaxTapChange  read
            Result := 0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.MaxTapChange;
        end;
        11:
        begin  // RegControls.MaxTapChange  write
            Set_Parameter('MaxTapChange', IntToStr(arg));
        end;
        12:
        begin  // RegControls.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].RegControls.ListSize;
        end;
        13:
        begin  // RegControls.TapNumber read
            Result := 0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.TapNum;  // tap number on the controlled-winding of the transformer controlled by this regcontrol
        end;
        14:
        begin  // RegControls.TapNumber write
            Set_Parameter('TapNum', IntToStr(arg));
        end
    else
        Result := -1;
    end;
end;

//***********************Floating poitn type properties***************************
function RegControlsF(mode: Longint; arg: Double): Double; CDECL;

var
    elem: TRegControlObj;

begin
    Result := 0.0;        // Default return value
    case mode of
        0:
        begin  // RegControls.CTPrimary read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.CT;
        end;
        1:
        begin  // RegControls.CTPrimary write
            Set_Parameter('CTprim', FloatToStr(arg));
        end;
        2:
        begin  // RegControls.PTRatio read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.PT;
        end;
        3:
        begin  // RegControls.PTRatio write
            Set_Parameter('PTratio', FloatToStr(arg));
        end;
        4:
        begin  // RegControls.ForwardR read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.LineDropR;
        end;
        5:
        begin  // RegControls.ForwardR write
            Set_Parameter('R', FloatToStr(arg));
        end;
        6:
        begin  // RegControls.ForwardX read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.LineDropX;
        end;
        7:
        begin  // RegControls.ForwardX write
            Set_Parameter('X', FloatToStr(arg));
        end;
        8:
        begin  // RegControls.ReverseR read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.RevLineDropR;
        end;
        9:
        begin  // RegControls.ReverseR write
            Set_Parameter('RevR', FloatToStr(arg));
        end;
        10:
        begin  // RegControls.ReverseX read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.RevLineDropX;
        end;
        11:
        begin  // RegControls.ReverseX write
            Set_Parameter('RevX', FloatToStr(arg));
        end;
        12:
        begin  // RegControls.Delay read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.InitialDelay;
        end;
        13:
        begin  // RegControls.Delay write
            Set_Parameter('Delay', FloatToStr(arg));
        end;
        14:
        begin  // RegControls.TapDelay read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.SubsequentDelay;
        end;
        15:
        begin  // RegControls.TapDelay write
            Set_Parameter('TapDelay', FloatToStr(arg));
        end;
        16:
        begin  // RegControls.VoltageLimit read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.VoltageLimit;
        end;
        17:
        begin  // RegControls.VoltageLimit write
            Set_Parameter('Vlimit', FloatToStr(arg));
        end;
        18:
        begin  // RegControls.ForwardBand read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.BandVoltage;
        end;
        19:
        begin  // RegControls.ForwardBand write
            Set_Parameter('Band', FloatToStr(arg));
        end;
        20:
        begin  // RegControls.ForwardVreg read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.TargetVoltage;
        end;
        21:
        begin  // RegControls.ForwardVreg write
            Set_Parameter('Vreg', FloatToStr(arg));
        end;
        22:
        begin  // RegControls.ReverseBand read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.RevBandVoltage;
        end;
        23:
        begin  // RegControls.ReverseBand Write
            Set_Parameter('RevBand', FloatToStr(arg));
        end;
        24:
        begin  // RegControls.ReverseVreg read
            Result := 0.0;
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := elem.RevTargetVoltage;
        end;
        25:
        begin  // RegControls.ReverseVreg write
            Set_Parameter('RevVreg', FloatToStr(arg));
        end
    else
        Result := -1.0;
    end;
end;

//********************String type properties**************************************
function RegControlsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    elem: TRegControlObj;
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    lst: TPointerList;

begin
    Result := pAnsiChar(Ansistring(''));  // Default return value
    case mode of
        0:
        begin  // RegControls.Name read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.Name));
        end;
        1:
        begin  // RegControls.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                lst := ActiveCircuit[ActiveActor].RegControls;
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
                    DoSimpleMsg('RegControl "' + S + '" Not Found in Active Circuit.', 5003);
                    elem := lst.Get(ActiveSave);    // Restore active Load
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
                end;
            end;
        end;
        2:
        begin  // RegControls.MonitoredBus read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.ControlledBusName));
        end;
        3:
        begin  // RegControls.MonitoredBus write
            Set_Parameter('Bus', Widestring(arg));
        end;
        4:
        begin  // RegControls.Transformer read
            Result := pAnsiChar(Ansistring(''));
            elem := ActiveRegControl;
            if elem <> NIL then
                Result := pAnsiChar(Ansistring(elem.Transformer.Name));
        end;
        5:
        begin  // RegControls.Transformer write
            Set_Parameter('Transformer', arg);
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//*******************Variant type properties**************************************
procedure RegControlsV(mode: Longint; out arg: Variant); CDECL;

var
    elem: TRegControlObj;
    lst: TPointerList;
    k: Integer;

begin
    case mode of
        0:
        begin  // RegControl.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    lst := RegControls;
                    if lst.ListSize > 0 then
                    begin
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
                end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
