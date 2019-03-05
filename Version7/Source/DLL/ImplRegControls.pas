unit ImplRegControls;

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
    TRegControls = class(TAutoObject, IRegControls)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_CTPrimary: Double; SAFECALL;
        function Get_Delay: Double; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_ForwardBand: Double; SAFECALL;
        function Get_ForwardR: Double; SAFECALL;
        function Get_ForwardVreg: Double; SAFECALL;
        function Get_ForwardX: Double; SAFECALL;
        function Get_IsInverseTime: Wordbool; SAFECALL;
        function Get_IsReversible: Wordbool; SAFECALL;
        function Get_MaxTapChange: Integer; SAFECALL;
        function Get_MonitoredBus: Widestring; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_PTratio: Double; SAFECALL;
        function Get_ReverseBand: Double; SAFECALL;
        function Get_ReverseR: Double; SAFECALL;
        function Get_ReverseVreg: Double; SAFECALL;
        function Get_ReverseX: Double; SAFECALL;
        function Get_TapDelay: Double; SAFECALL;
        function Get_TapWinding: Integer; SAFECALL;
        function Get_Transformer: Widestring; SAFECALL;
        function Get_VoltageLimit: Double; SAFECALL;
        function Get_Winding: Integer; SAFECALL;
        function Get_TapNumber: Integer; SAFECALL;
        procedure Set_CTPrimary(Value: Double); SAFECALL;
        procedure Set_Delay(Value: Double); SAFECALL;
        procedure Set_ForwardBand(Value: Double); SAFECALL;
        procedure Set_ForwardR(Value: Double); SAFECALL;
        procedure Set_ForwardVreg(Value: Double); SAFECALL;
        procedure Set_ForwardX(Value: Double); SAFECALL;
        procedure Set_IsInverseTime(Value: Wordbool); SAFECALL;
        procedure Set_IsReversible(Value: Wordbool); SAFECALL;
        procedure Set_MaxTapChange(Value: Integer); SAFECALL;
        procedure Set_MonitoredBus(const Value: Widestring); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        procedure Set_PTratio(Value: Double); SAFECALL;
        procedure Set_ReverseBand(Value: Double); SAFECALL;
        procedure Set_ReverseR(Value: Double); SAFECALL;
        procedure Set_ReverseVreg(Value: Double); SAFECALL;
        procedure Set_ReverseX(Value: Double); SAFECALL;
        procedure Set_TapDelay(Value: Double); SAFECALL;
        procedure Set_TapWinding(Value: Integer); SAFECALL;
        procedure Set_Transformer(const Value: Widestring); SAFECALL;
        procedure Set_VoltageLimit(Value: Double); SAFECALL;
        procedure Set_Winding(Value: Integer); SAFECALL;
        procedure Set_TapNumber(Value: Integer); SAFECALL;

        function Get_Count: Integer; SAFECALL;
        procedure Reset; SAFECALL;

    end;

implementation

uses
    ComServ,
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
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.RegControls.Active;
end;

procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('regcontrol.%s.%s=%s', [ActiveRegControl.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;

function TRegControls.Get_AllNames: Olevariant;
var
    elem: TRegControlObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            lst := RegControls;
            if lst.ListSize > 0 then
            begin
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
end;

function TRegControls.Get_CTPrimary: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.CT;
end;

function TRegControls.Get_Delay: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.InitialDelay;
end;

function TRegControls.Get_First: Integer;
var
    elem: TRegControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.RegControls;
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

function TRegControls.Get_ForwardBand: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.BandVoltage;
end;

function TRegControls.Get_ForwardR: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.LineDropR;
end;

function TRegControls.Get_ForwardVreg: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.TargetVoltage;
end;

function TRegControls.Get_ForwardX: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.LineDropX;
end;

function TRegControls.Get_IsInverseTime: Wordbool;
var
    elem: TRegControlObj;
begin
    Result := FALSE;
    elem := ActiveRegControl;
    if elem <> NIL then
        if elem.IsInverseTime then
            Result := TRUE;
end;

function TRegControls.Get_IsReversible: Wordbool;
var
    elem: TRegControlObj;
begin
    Result := FALSE;
    elem := ActiveRegControl;
    if elem <> NIL then
        if elem.UseReverseDrop then
            Result := TRUE;
end;

function TRegControls.Get_MaxTapChange: Integer;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.MaxTapChange;
end;

function TRegControls.Get_MonitoredBus: Widestring;
var
    elem: TRegControlObj;
begin
    Result := '';
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.ControlledBusName;
end;

function TRegControls.Get_Name: Widestring;
var
    elem: TRegControlObj;
begin
    Result := '';
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.Name;
end;

function TRegControls.Get_Next: Integer;
var
    elem: TRegControlObj;
    lst: TPointerList;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.RegControls;
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

function TRegControls.Get_PTratio: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.PT;
end;

function TRegControls.Get_ReverseBand: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevBandVoltage;
end;

function TRegControls.Get_ReverseR: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevLineDropR;
end;

function TRegControls.Get_ReverseVreg: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevTargetVoltage;
end;

function TRegControls.Get_ReverseX: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevLineDropX;
end;

function TRegControls.Get_TapDelay: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.SubsequentDelay;
end;

function TRegControls.Get_TapWinding: Integer;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.TrWinding;  // has the taps
end;

function TRegControls.Get_Transformer: Widestring;
var
    elem: TRegControlObj;
begin
    Result := '';
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.Transformer.Name;
end;

function TRegControls.Get_VoltageLimit: Double;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.VoltageLimit;
end;

function TRegControls.Get_Winding: Integer;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.ElementTerminal;  // monitored winding
end;

function TRegControls.Get_TapNumber: Integer;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.TapNum;  // tap number on the controlled-winding of the transformer controlled by this regcontrol
end;


procedure TRegControls.Set_CTPrimary(Value: Double);
begin
    Set_Parameter('CTprim', FloatToStr(Value));
end;

procedure TRegControls.Set_Delay(Value: Double);
begin
    Set_Parameter('Delay', FloatToStr(Value));
end;

procedure TRegControls.Set_ForwardBand(Value: Double);
begin
    Set_Parameter('Band', FloatToStr(Value));
end;

procedure TRegControls.Set_ForwardR(Value: Double);
begin
    Set_Parameter('R', FloatToStr(Value));
end;

procedure TRegControls.Set_ForwardVreg(Value: Double);
begin
    Set_Parameter('Vreg', FloatToStr(Value));
end;

procedure TRegControls.Set_ForwardX(Value: Double);
begin
    Set_Parameter('X', FloatToStr(Value));
end;

procedure TRegControls.Set_IsInverseTime(Value: Wordbool);
begin
    if Value = TRUE then
        Set_Parameter('InverseTime', 'y')
    else
        Set_Parameter('InverseTime', 'n');
end;

procedure TRegControls.Set_IsReversible(Value: Wordbool);
begin
    if Value = TRUE then
        Set_Parameter('Reversible', 'y')
    else
        Set_Parameter('Reversible', 'n');
end;

procedure TRegControls.Set_MaxTapChange(Value: Integer);
begin
    Set_Parameter('MaxTapChange', IntToStr(Value));
end;

procedure TRegControls.Set_MonitoredBus(const Value: Widestring);
begin
    Set_Parameter('Bus', Value);
end;

procedure TRegControls.Set_Name(const Value: Widestring);
var
    ActiveSave: Integer;
    S: String;
    Found: Boolean;
    elem: TRegControlObj;
    lst: TPointerList;
begin
    if ActiveCircuit <> NIL then
    begin
        lst := ActiveCircuit.RegControls;
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
            DoSimpleMsg('RegControl "' + S + '" Not Found in Active Circuit.', 5003);
            elem := lst.Get(ActiveSave);    // Restore active Load
            ActiveCircuit.ActiveCktElement := elem;
        end;
    end;
end;

procedure TRegControls.Set_PTratio(Value: Double);
begin
    Set_Parameter('PTratio', FloatToStr(Value));
end;

procedure TRegControls.Set_ReverseBand(Value: Double);
begin
    Set_Parameter('RevBand', FloatToStr(Value));
end;

procedure TRegControls.Set_ReverseR(Value: Double);
begin
    Set_Parameter('RevR', FloatToStr(Value));
end;

procedure TRegControls.Set_ReverseVreg(Value: Double);
begin
    Set_Parameter('RevVreg', FloatToStr(Value));
end;

procedure TRegControls.Set_ReverseX(Value: Double);
begin
    Set_Parameter('RevX', FloatToStr(Value));
end;

procedure TRegControls.Set_TapDelay(Value: Double);
begin
    Set_Parameter('TapDelay', FloatToStr(Value));
end;

procedure TRegControls.Set_TapWinding(Value: Integer);
begin
    Set_Parameter('TapWinding', IntToStr(Value));
end;

procedure TRegControls.Set_Transformer(const Value: Widestring);
begin
    Set_Parameter('Transformer', Value);
end;

procedure TRegControls.Set_VoltageLimit(Value: Double);
begin
    Set_Parameter('Vlimit', FloatToStr(Value));
end;

procedure TRegControls.Set_Winding(Value: Integer);
begin
    Set_Parameter('Winding', IntToStr(Value));
end;

procedure TRegControls.Set_TapNumber(Value: Integer);
begin
    Set_Parameter('TapNum', IntToStr(Value));
end;

function TRegControls.Get_Count: Integer;
begin
    if Assigned(Activecircuit) then
        Result := ActiveCircuit.RegControls.ListSize;
end;

procedure TRegControls.Reset;
var
    elem: TRegControlObj;
begin
    elem := ActiveRegControl;
    if elem <> NIL then
    begin
        elem.Reset;
    end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TRegControls, Class_RegControls,
        ciInternal, tmApartment);
end.
