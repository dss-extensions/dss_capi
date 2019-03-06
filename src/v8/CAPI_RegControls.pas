unit CAPI_RegControls;

{$inline on}

interface

uses
    CAPI_Utils;

procedure RegControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure RegControls_Get_AllNames_GR(); CDECL;
function RegControls_Get_CTPrimary(): Double; CDECL;
function RegControls_Get_Delay(): Double; CDECL;
function RegControls_Get_First(): Integer; CDECL;
function RegControls_Get_ForwardBand(): Double; CDECL;
function RegControls_Get_ForwardR(): Double; CDECL;
function RegControls_Get_ForwardVreg(): Double; CDECL;
function RegControls_Get_ForwardX(): Double; CDECL;
function RegControls_Get_IsInverseTime(): Wordbool; CDECL;
function RegControls_Get_IsReversible(): Wordbool; CDECL;
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
procedure RegControls_Set_IsInverseTime(Value: Wordbool); CDECL;
procedure RegControls_Set_IsReversible(Value: Wordbool); CDECL;
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
    PointerList;

function ActiveRegControl: TRegControlObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].RegControls.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('regcontrol.%s.%s=%s', [ActiveRegControl.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure RegControls_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    elem: TRegControlObj;
    lst: TPointerList;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
        begin
            lst := RegControls;
            if lst.ListSize > 0 then
            begin
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
end;

procedure RegControls_Get_AllNames_GR(); CDECL;
// Same as RegControls_Get_AllNames but uses global result (GR) pointers
begin
    RegControls_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function RegControls_Get_CTPrimary(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.CT;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Delay(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.InitialDelay;
end;
//------------------------------------------------------------------------------
function RegControls_Get_First(): Integer; CDECL;
var
    elem: TRegControlObj;
    lst: TPointerList;
begin
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
//------------------------------------------------------------------------------
function RegControls_Get_ForwardBand(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.BandVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardR(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.LineDropR;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardVreg(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.TargetVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ForwardX(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.LineDropX;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsInverseTime(): Wordbool; CDECL;
var
    elem: TRegControlObj;
begin
    Result := FALSE;
    elem := ActiveRegControl;
    if elem <> NIL then
        if elem.IsInverseTime then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function RegControls_Get_IsReversible(): Wordbool; CDECL;
var
    elem: TRegControlObj;
begin
    Result := FALSE;
    elem := ActiveRegControl;
    if elem <> NIL then
        if elem.UseReverseDrop then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MaxTapChange(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.MaxTapChange;
end;
//------------------------------------------------------------------------------
function RegControls_Get_MonitoredBus_AnsiString(): Ansistring; inline;
var
    elem: TRegControlObj;
begin
    Result := '';
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.ControlledBusName;
end;

function RegControls_Get_MonitoredBus(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(RegControls_Get_MonitoredBus_AnsiString());
end;
//------------------------------------------------------------------------------
function RegControls_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TRegControlObj;
begin
    Result := '';
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.Name;
end;

function RegControls_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(RegControls_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function RegControls_Get_Next(): Integer; CDECL;
var
    elem: TRegControlObj;
    lst: TPointerList;
begin
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
//------------------------------------------------------------------------------
function RegControls_Get_PTratio(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.PT;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseBand(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevBandVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseR(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevLineDropR;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseVreg(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevTargetVoltage;
end;
//------------------------------------------------------------------------------
function RegControls_Get_ReverseX(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.RevLineDropX;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapDelay(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.SubsequentDelay;
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapWinding(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.TrWinding;  // has the taps
end;
//------------------------------------------------------------------------------
function RegControls_Get_Transformer_AnsiString(): Ansistring; inline;
var
    elem: TRegControlObj;
begin
    Result := '';
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.Transformer.Name;
end;

function RegControls_Get_Transformer(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(RegControls_Get_Transformer_AnsiString());
end;
//------------------------------------------------------------------------------
function RegControls_Get_VoltageLimit(): Double; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0.0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.VoltageLimit;
end;
//------------------------------------------------------------------------------
function RegControls_Get_Winding(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.ElementTerminal;  // monitored winding
end;
//------------------------------------------------------------------------------
function RegControls_Get_TapNumber(): Integer; CDECL;
var
    elem: TRegControlObj;
begin
    Result := 0;
    elem := ActiveRegControl;
    if elem <> NIL then
        Result := elem.TapNum;  // tap number on the controlled-winding of the transformer controlled by this regcontrol
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_CTPrimary(Value: Double); CDECL;
begin
    Set_Parameter('CTprim', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Delay(Value: Double); CDECL;
begin
    Set_Parameter('Delay', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardBand(Value: Double); CDECL;
begin
    Set_Parameter('Band', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardR(Value: Double); CDECL;
begin
    Set_Parameter('R', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardVreg(Value: Double); CDECL;
begin
    Set_Parameter('Vreg', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ForwardX(Value: Double); CDECL;
begin
    Set_Parameter('X', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsInverseTime(Value: Wordbool); CDECL;
begin
    if Value = TRUE then
        Set_Parameter('InverseTime', 'y')
    else
        Set_Parameter('InverseTime', 'n');
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_IsReversible(Value: Wordbool); CDECL;
begin
    if Value = TRUE then
        Set_Parameter('Reversible', 'y')
    else
        Set_Parameter('Reversible', 'n');
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MaxTapChange(Value: Integer); CDECL;
begin
    Set_Parameter('MaxTapChange', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_MonitoredBus(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('Bus', Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    if RegControlClass[ActiveActor].SetActive(Value) then
    begin
        ActiveCircuit[ActiveActor].ActiveCktElement := RegControlClass[ActiveActor].ElementList.Active;
        ActiveCircuit[ActiveActor].RegControls.Get(RegControlClass[ActiveActor].Active);
    end
    else
    begin
        DoSimpleMsg('RegControl "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_PTratio(Value: Double); CDECL;
begin
    Set_Parameter('PTratio', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseBand(Value: Double); CDECL;
begin
    Set_Parameter('RevBand', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseR(Value: Double); CDECL;
begin
    Set_Parameter('RevR', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseVreg(Value: Double); CDECL;
begin
    Set_Parameter('RevVreg', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_ReverseX(Value: Double); CDECL;
begin
    Set_Parameter('RevX', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapDelay(Value: Double); CDECL;
begin
    Set_Parameter('TapDelay', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapWinding(Value: Integer); CDECL;
begin
    Set_Parameter('TapWinding', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Transformer(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('Transformer', Value);
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_VoltageLimit(Value: Double); CDECL;
begin
    Set_Parameter('Vlimit', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_Winding(Value: Integer); CDECL;
begin
    Set_Parameter('Winding', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_TapNumber(Value: Integer); CDECL;
begin
    Set_Parameter('TapNum', IntToStr(Value));
end;
//------------------------------------------------------------------------------
function RegControls_Get_Count(): Integer; CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].RegControls.ListSize;
end;
//------------------------------------------------------------------------------
procedure RegControls_Reset(); CDECL;
var
    elem: TRegControlObj;
begin
    elem := ActiveRegControl;
    if elem <> NIL then
    begin
        elem.Reset;
    end;

end;
//------------------------------------------------------------------------------
function RegControls_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].RegControls.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure RegControls_Set_idx(Value: Integer); CDECL;
var
    pRegControl: TRegControlObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    pRegControl := ActiveCircuit[ActiveActor].RegControls.Get(Value);
    if pRegControl = NIL then
    begin
        DoSimpleMsg('Invalid RegControl index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit[ActiveActor].ActiveCktElement := pRegControl;
end;
//------------------------------------------------------------------------------
end.
