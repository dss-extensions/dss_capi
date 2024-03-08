unit CAPI_CktElement;

interface

uses
    CAPI_Utils,
    CktElement,
    CAPI_Types;

procedure CktElement_Get_BusNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_BusNames_GR(); CDECL;
function CktElement_Get_Name(): PAnsiChar; CDECL;
function CktElement_Get_NumConductors(): Integer; CDECL;
function CktElement_Get_NumPhases(): Integer; CDECL;
function CktElement_Get_NumTerminals(): Integer; CDECL;
procedure CktElement_Set_BusNames(ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
procedure CktElement_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_Currents_GR(); CDECL;
procedure CktElement_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_Voltages_GR(); CDECL;
function CktElement_Get_EmergAmps(): Double; CDECL;
function CktElement_Get_Enabled(): TAPIBoolean; CDECL;
procedure CktElement_Get_Losses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_Losses_GR(); CDECL;
function CktElement_Get_NormalAmps(): Double; CDECL;
procedure CktElement_Get_PhaseLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_PhaseLosses_GR(); CDECL;
procedure CktElement_Get_Powers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_Powers_GR(); CDECL;
procedure CktElement_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_SeqCurrents_GR(); CDECL;
procedure CktElement_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_SeqPowers_GR(); CDECL;
procedure CktElement_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_SeqVoltages_GR(); CDECL;
procedure CktElement_Close(Term, Phs: Integer); CDECL;
procedure CktElement_Open(Term, Phs: Integer); CDECL;
procedure CktElement_Set_EmergAmps(Value: Double); CDECL;
procedure CktElement_Set_Enabled(Value: TAPIBoolean); CDECL;
procedure CktElement_Set_NormalAmps(Value: Double); CDECL;
function CktElement_IsOpen(Term, Phs: Integer): TAPIBoolean; CDECL;
procedure CktElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_AllPropertyNames_GR(); CDECL;
function CktElement_Get_NumProperties(): Integer; CDECL;
procedure CktElement_Get_Residuals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_Residuals_GR(); CDECL;
procedure CktElement_Get_Yprim(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_Yprim_GR(); CDECL;
function CktElement_Get_DisplayName(): PAnsiChar; CDECL;
function CktElement_Get_GUID(): PAnsiChar; CDECL;
function CktElement_Get_Handle(): Integer; CDECL;
procedure CktElement_Set_DisplayName(const Value: PAnsiChar); CDECL;
function CktElement_Get_Controller(idx: Integer): PAnsiChar; CDECL;
function CktElement_Get_EnergyMeter(): PAnsiChar; CDECL;
function CktElement_Get_HasVoltControl(): TAPIBoolean; CDECL;
function CktElement_Get_HasSwitchControl(): TAPIBoolean; CDECL;
procedure CktElement_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_CplxSeqVoltages_GR(); CDECL;
procedure CktElement_Get_CplxSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_CplxSeqCurrents_GR(); CDECL;
procedure CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_AllVariableNames_GR(); CDECL;
procedure CktElement_Get_AllVariableValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_AllVariableValues_GR(); CDECL;
function CktElement_Get_Variable(const VarName: PAnsiChar; out Code: Integer): Double; CDECL;
function CktElement_Get_Variablei(Idx: Integer; out Code: Integer): Double; CDECL;
procedure CktElement_Set_Variable(const VarName: PAnsiChar; out Code: Integer; Value: Double); CDECL;
procedure CktElement_Set_Variablei(Idx: Integer; out Code: Integer; Value: Double); CDECL;
procedure CktElement_Get_NodeOrder(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_NodeOrder_GR(); CDECL;
function CktElement_Get_HasOCPDevice(): TAPIBoolean; CDECL;
function CktElement_Get_NumControls(): Integer; CDECL;
function CktElement_Get_OCPDevIndex(): Integer; CDECL;
function CktElement_Get_OCPDevType(): Integer; CDECL;
procedure CktElement_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_CurrentsMagAng_GR(); CDECL;
procedure CktElement_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_VoltagesMagAng_GR(); CDECL;
procedure CktElement_Get_TotalPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_TotalPowers_GR(); CDECL;

//
// These are the same as CktElement_Get/Set_Variable and CktElement_Get/Set_Variablei.
//
//function CktElement_Get_VariableByName
//procedure CktElement_Set_VariableByName
//function CktElement_Get_VariableByIndex
//procedure CktElement_Set_VariableByIndex

function CktElement_Get_VariableName(): PAnsiChar; CDECL;
procedure CktElement_Set_VariableName(const Value: PAnsiChar); CDECL;
function CktElement_Get_VariableValue(): Double; CDECL;
procedure CktElement_Set_VariableValue(Value: Double); CDECL;
function CktElement_Get_VariableIdx(): Integer; CDECL;
procedure CktElement_Set_VariableIdx(Value: Integer); CDECL;

// API Extensions
function CktElement_Get_IsIsolated(): TAPIBoolean; CDECL;
procedure CktElement_Get_NodeRef(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure CktElement_Get_NodeRef_GR(); CDECL;

// API Extensions -- Obj API
function Obj_CktElement_MaxCurrent(obj: TDSSCktElement; terminalIdx: Integer): Double; CDECL;
procedure Obj_Circuit_Set_ActiveCktElement(obj: TDSSCktElement); CDECL;

function CktElement_Get_Pointer(): Pointer; CDECL;

implementation

uses
    CAPI_Alt,
    CAPI_Constants,
    DSSClassDefs,
    DSSGlobals,
    UComplex, DSSUcomplex,
    Sysutils,
    PDElement,
    PCElement,
    MathUtil,
    Utilities,
    DSSClass,
    DSSHelper,
    Bus,
    Solution;

//------------------------------------------------------------------------------
function IsPDElement(elem: TDSSCktElement): Boolean;
begin
    Result := ((elem.DSSObjType and 3) = PD_ELEMENT)
end;
//------------------------------------------------------------------------------
function InvalidCktElement(DSS: TDSSContext; out elem: TDSSCktElement; const NeedsPCElement: Boolean=False): Boolean; inline;
begin
    Result := InvalidCircuit(DSS);
    elem := NIL;
    if Result then
        Exit;
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    Result := (elem = NIL);
    if Result and DSS_CAPI_EXT_ERRORS then
    begin
        DoSimpleMsg(DSS, _('No active circuit element found! Activate one and retry.'), 97800);
    end;
    if NeedsPCElement and ((elem.DSSObjType and BASECLASSMASK) <> PC_ELEMENT) then
    begin
        if DSS_CAPI_EXT_ERRORS then
            DoSimpleMsg(DSS, _('The active circuit element is not a PC Element'), 100004);
        Result := True;
        Exit;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_BusNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
    Alt_CE_Get_BusNames(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_BusNames_GR(); CDECL;
// Same as CktElement_Get_BusNames but uses global result (GR) pointers
begin
    CktElement_Get_BusNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_Name(): PAnsiChar; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.FullName);
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumConductors(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Result := elem.NConds
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumPhases(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Result := elem.NPhases
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumTerminals(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        Result := 0;
        Exit;
    end;
    Result := elem.NTerms
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_BusNames(ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Alt_CE_Set_BusNames(elem, ValuePtr, ValueCount);
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
        
    Alt_CE_Get_Currents(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_Currents_GR(); CDECL;
// Same as CktElement_Get_Currents but uses global result (GR) pointers
begin
    CktElement_Get_Currents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Bus Voltages at active terminal
var
    elem: TDSSCktElement;
begin
    // Return voltages for all terminals
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    Alt_CE_Get_Voltages(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_Voltages_GR(); CDECL;
// Same as CktElement_Get_Voltages but uses global result (GR) pointers
begin
    CktElement_Get_Voltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_EmergAmps(): Double; CDECL;
var
    elem: TDSSCktElement;
    pd: TPDElement;
begin
    Result := 0;
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    if not IsPDElement(elem) then
        Exit;

    pd := (elem as TPDElement);
    Result := pd.EmergAmps;
end;

//------------------------------------------------------------------------------
function CktElement_Get_Enabled(): TAPIBoolean; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := FALSE;
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    Result := elem.Enabled;
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Losses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Alt_CE_Get_Losses(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_Losses_GR(); CDECL;
// Same as CktElement_Get_Losses but uses global result (GR) pointers
begin
    CktElement_Get_Losses(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_NormalAmps(): Double; CDECL;
var
    elem: TDSSCktElement;
    pd: TPDElement;
begin
    Result := 0;
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    if not IsPDElement(elem) then
        Exit;

    pd := (elem as TPDElement);
    Result := pd.NormAmps;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_PhaseLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Returns Phase losses in kW, kVar
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_CE_Get_PhaseLosses(ResultPtr, ResultCount, elem);
end;


procedure CktElement_Get_PhaseLosses_GR(); CDECL;
// Same as CktElement_Get_PhaseLosses but uses global result (GR) pointers
begin
    CktElement_Get_PhaseLosses(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Powers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return complex kW, kvar in each conductor for each terminal
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_CE_Get_Powers(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_Powers_GR(); CDECL;
// Same as CktElement_Get_Powers but uses global result (GR) pointers
begin
    CktElement_Get_Powers(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// All sequence currents of active ciruit element
// returns magnitude only.
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Alt_CE_Get_SeqCurrents(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_SeqCurrents_GR(); CDECL;
// Same as CktElement_Get_SeqCurrents but uses global result (GR) pointers
begin
    CktElement_Get_SeqCurrents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// All seq Powers of active 3-phase circuit element
// returns kW + j kvar
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) then // or (not elem.Enabled)
        Exit;

    Alt_CE_Get_SeqPowers(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_SeqPowers_GR(); CDECL;
// Same as CktElement_Get_SeqPowers but uses global result (GR) pointers
begin
    CktElement_Get_SeqPowers(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// All voltages of active circuit element
// magnitude only
// returns a set of seq voltages (3) for each terminal
// 0, 1, 2 sequence  (0, +, -)
var
    Result: PDoubleArray0;
    i: Integer;
    V012: pComplexArray;
    S: String;
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    Alt_CE_Get_SeqVoltages(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_SeqVoltages_GR(); CDECL;
// Same as CktElement_Get_SeqVoltages but uses global result (GR) pointers
begin
    CktElement_Get_SeqVoltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Close(Term, Phs: Integer); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    Alt_CE_Close(elem, Term, Phs);
end;
//------------------------------------------------------------------------------
procedure CktElement_Open(Term, Phs: Integer); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    Alt_CE_Open(elem, Term, Phs);
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_EmergAmps(Value: Double); CDECL;
var
    elem: TDSSCktElement;
    pd: TPDElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    if IsPDElement(elem) then
    begin
        pd := elem as TPDElement;
        pd.EmergAmps := Value;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_Enabled(Value: TAPIBoolean); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    elem.Enabled := Value;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_NormalAmps(Value: Double); CDECL;
var
    elem: TDSSCktElement;
    pd: TPDElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    if IsPDElement(elem) then
    begin
        pd := elem as TPDElement;
        pd.NormAmps := Value;
    end;
end;
//------------------------------------------------------------------------------
function CktElement_IsOpen(Term, Phs: Integer): TAPIBoolean; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := False;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    Result := Alt_CE_IsOpen(elem, Term, Phs);    
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
    
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, elem.ParentClass.NumProperties);
    for k := 1 to elem.ParentClass.NumProperties do
    begin
        Result[k - 1] := DSS_CopyStringAsPChar(elem.ParentClass.PropertyName[k]);
    end;
end;

procedure CktElement_Get_AllPropertyNames_GR(); CDECL;
// Same as CktElement_Get_AllPropertyNames but uses global result (GR) pointers
begin
    CktElement_Get_AllPropertyNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_NumProperties(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := 0;
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    
    Result := elem.ParentClass.NumProperties;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_Residuals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    cBuffer: pComplexArray;
    iV, i, j, k: Integer;
    cResid: Complex;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * elem.NTerms, 2, elem.NTerms);    // 2 values per terminal
    cBuffer := Allocmem(sizeof(Complex) * elem.Yorder);
    elem.GetCurrents(cBuffer);
    iV := 0;
    for i := 1 to elem.NTerms do
    begin
        cResid := 0;
        k := (i - 1) * elem.Nconds;
        for j := 1 to elem.Nconds do
        begin
            inc(k);
            cResid += CBuffer[k];
        end;
        Result[iV] := Cabs(cResid);
        Inc(iV);
        Result[iV] := CDang(cResid);
        Inc(iV);
    end;
    Reallocmem(cBuffer, 0);
end;

procedure CktElement_Get_Residuals_GR(); CDECL;
// Same as CktElement_Get_Residuals but uses global result (GR) pointers
begin
    CktElement_Get_Residuals(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Yprim(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    cValues: pComplexArray;
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    cValues := elem.GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
    if cValues = NIL then 
        Exit;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * elem.Yorder * elem.Yorder, elem.Yorder, elem.Yorder);
    Move(cValues^, ResultPtr^, ResultCount^ * SizeOf(Double));
end;

procedure CktElement_Get_Yprim_GR(); CDECL;
// Same as CktElement_Get_Yprim but uses global result (GR) pointers
begin
    CktElement_Get_Yprim(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_DisplayName(): PAnsiChar; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    if elem.DisplayName <> '' then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DisplayName)
    else
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.ParentClass.Name + '_' + elem.Name);
end;

//------------------------------------------------------------------------------
function CktElement_Get_GUID(): PAnsiChar; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.ID)
end;

//------------------------------------------------------------------------------
function CktElement_Get_Handle(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Result := elem.Handle
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_DisplayName(const Value: PAnsiChar); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    elem.DisplayName := Value;
end;
//------------------------------------------------------------------------------
function CktElement_Get_Controller(idx: Integer): PAnsiChar; CDECL;
var
    elem: TDSSCktElement;
    ctrl: TDSSCktElement;
begin
    Result := NIL;

    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    if (idx > 0) and (idx <= elem.ControlElementList.Count) then
    begin
        ctrl := elem.ControlElementList.Get(idx);
        if ctrl <> NIL then
            Result := DSS_GetAsPAnsiChar(DSSPrime, ctrl.FullName);
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_EnergyMeter(): PAnsiChar; CDECL;
var
    pd: TPDElement;
    elem: TDSSCktElement;
begin
    Result := NIL;

    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    if Flg.HasEnergyMeter in elem.Flags then
    begin
        pd := elem as TPDElement;
        Result := DSS_GetAsPAnsiChar(DSSPrime, pd.MeterObj.Name);
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasVoltControl(): TAPIBoolean; CDECL;
// Returns true if any of the controls is a capcontrol or a regcontrol
var
    ctrl: TDSSCktElement;
    elem: TDSSCktElement;
begin
    Result := FALSE;

    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    for ctrl in elem.ControlElementlist do
    begin
        case (ctrl.DSSObjType and CLASSMASK) of
            CAP_CONTROL,
            REG_CONTROL:
            begin
                Result := TRUE;
                Exit;
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasSwitchControl(): TAPIBoolean; CDECL;
var
    ctrl: TDSSCktElement;
    elem: TDSSCktElement;
begin
    Result := FALSE;

    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    for ctrl in elem.ControlElementList do
    begin
        case (ctrl.DSSObjType and CLASSMASK) of
            SWT_CONTROL:
            begin
                Result := TRUE;
                Exit;
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Alt_CE_Get_ComplexSeqVoltages(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_CplxSeqVoltages_GR(); CDECL;
// Same as CktElement_Get_CplxSeqVoltages but uses global result (GR) pointers
begin
    CktElement_Get_CplxSeqVoltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_CplxSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    Alt_CE_Get_ComplexSeqCurrents(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_CplxSeqCurrents_GR(); CDECL;
// Same as CktElement_Get_CplxSeqCurrents but uses global result (GR) pointers
begin
    CktElement_Get_CplxSeqCurrents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem, True) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
    Alt_PCE_Get_VariableNames(ResultPtr, ResultCount, elem as TPCElement);
end;

procedure CktElement_Get_AllVariableNames_GR(); CDECL;
// Same as CktElement_Get_AllVariableNames but uses global result (GR) pointers
begin
    CktElement_Get_AllVariableNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_AllVariableValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return array of doubles with values of all variables if PCElement
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCktElement(DSSPrime, elem, True) then
        Exit;
    Alt_PCE_Get_VariableValues(ResultPtr, ResultCount, elem as TPCElement);
end;

procedure CktElement_Get_AllVariableValues_GR(); CDECL;
// Same as CktElement_Get_AllVariableValues but uses global result (GR) pointers
begin
    CktElement_Get_AllVariableValues(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_Variable(const VarName: PAnsiChar; out Code: Integer): Double; CDECL; //TODO: Remove Code and use Error interface?
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
    VarIndex: Integer;
begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;
    
    elem := (cktelem as TPCElement);
    VarIndex := elem.LookupVariable(VarName);
    if (VarIndex > 0) and (VarIndex <= elem.NumVariables) then
    begin
        Result := elem.Variable[VarIndex];
        Code := 0;  // Signify result is OK.
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_Variablei(Idx: Integer; out Code: Integer): Double; CDECL;
// Get Value of a variable by index
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;
    
    elem := (cktelem as TPCElement);
    if (Idx > 0) and (Idx <= elem.NumVariables) then
    begin
        Result := elem.Variable[Idx];
        Code := 0;  // Signify result is OK.
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_Variable(const VarName: PAnsiChar; out Code: Integer; Value: Double); CDECL; //TODO: Remove Code and use Error interface?
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
    VarIndex: Integer;
begin
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;
    
    elem := (cktelem as TPCElement);
    VarIndex := elem.LookupVariable(VarName);
    if (VarIndex > 0) and (VarIndex <= elem.NumVariables) then
    begin
        elem.Variable[VarIndex] := Value;
        Code := 0;  // Signify result is OK.
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_Variablei(Idx: Integer; out Code: Integer; Value: Double); CDECL;
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
begin
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;
    
    elem := (cktelem as TPCElement);
    if (Idx > 0) and (Idx <= elem.NumVariables) then
    begin
        elem.Variable[Idx] := Value;
        Code := 0;  // Signify result is OK.
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_NodeOrder(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray0;
    k: Integer;
    i: Integer;
    j: Integer;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        // Just ignore as the original code did
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    if elem.NodeRef = NIL then
    begin
        // Warn and exit
        DoSimpleMsg(DSSPrime, _('Nodes are not initialized. Try solving the system first.'), 15013);
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, elem.NTerms * elem.Nconds, elem.NTerms, elem.Nconds);
    k := 0;
    for i := 1 to elem.Nterms do
    begin
        for j := (i - 1) * elem.NConds + 1 to i * elem.Nconds do
        begin
            Result[k] := GetNodeNum(DSSPrime, elem.NodeRef[j]);
            inc(k);
        end;
    end;
end;

procedure CktElement_Get_NodeOrder_GR(); CDECL;
// Same as CktElement_Get_NodeOrder but uses global result (GR) pointers
begin
    CktElement_Get_NodeOrder(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasOCPDevice(): TAPIBoolean; CDECL;
// Check for presence of a fuse, recloser, etc.
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        Result := FALSE;
        Exit;
    end;
    Result := Flg.HasOCPDevice in elem.Flags;
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumControls(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        Result := 0;
        Exit;
    end;
    Result := elem.ControlElementList.Count;
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevIndex(): Integer; CDECL;
var
    iControl: Integer;
    ctrl: TDSSCktElement;
    elem: TDSSCktElement;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    iControl := 1;
    repeat
        // cycle through the list of controls until we find a fuse, recloser, or relay
        ctrl := elem.ControlElementList.Get(iControl);
        if ctrl <> NIL then
            case (ctrl.DSSObjType and CLASSMASK) of
                FUSE_CONTROL:
                    Result := iControl;
                RECLOSER_CONTROL:
                    Result := iControl;
                RELAY_CONTROL:
                    Result := iControl;
            end;
        inc(iControl);
    until (iControl > elem.ControlElementList.Count) or (Result > 0);
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevType(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        Result := 0;
        Exit;
    end;
    Result := GetOCPDeviceType(elem);     // see Utilities.pas
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// return currents in magnitude, angle array
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_CE_Get_CurrentsMagAng(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_CurrentsMagAng_GR(); CDECL;
// Same as CktElement_Get_CurrentsMagAng but uses global result (GR) pointers
begin
    CktElement_Get_CurrentsMagAng(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Bus Voltages in magnitude, angle at all terminal
var
    elem: TDSSCktElement;
begin
    // Return voltages for all terminals
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_CE_Get_VoltagesMagAng(ResultPtr, ResultCount, elem);
end;

procedure CktElement_Get_VoltagesMagAng_GR(); CDECL;
// Same as CktElement_Get_VoltagesMagAng but uses global result (GR) pointers
begin
    CktElement_Get_VoltagesMagAng(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
function CktElement_Get_IsIsolated(): TAPIBoolean; CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        Result := FALSE;
        Exit;
    end;

    Result := Flg.IsIsolated in elem.Flags;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_TotalPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Exit;
    end;
    Alt_CE_Get_TotalPowers(ResultPtr, ResultCount, elem);
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_TotalPowers_GR(); CDECL;
// Same as CktElement_Get_TotalPowers but uses global result (GR) pointers
begin
    CktElement_Get_TotalPowers(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_NodeRef(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;    
var
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCktElement(DSSPrime, elem) then 
        Exit;
        
    if elem.NodeRef = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('NodeRef is not populated for the current element!'), 97801);
        end;
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, elem.Yorder);
    Move(elem.NodeRef[1], ResultPtr^, elem.Yorder * SizeOf(Integer));
end;

procedure CktElement_Get_NodeRef_GR(); CDECL;
// Same as CktElement_Get_NodeRef but uses global result (GR) pointers
begin
    CktElement_Get_NodeRef(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;
//------------------------------------------------------------------------------
function CktElement_Get_VariableName(): PAnsiChar; CDECL;
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;

    elem := TPCElement(cktelem);
    if (DSSPrime.API_VarIdx <= 0) or (DSSPrime.API_VarIdx > elem.NumVariables) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid variable index %d for "%s"', [DSSPrime.API_VarIdx, elem.FullName], 97802);
        Exit;
    end;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.VariableName(DSSPrime.API_VarIdx));
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_VariableName(const Value: PAnsiChar); CDECL;
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
begin
    if InvalidCktElement(DSSPrime, cktelem, True) then
    begin
        DSSPrime.API_VarIdx := -1;
        Exit;
    end;
    elem := TPCElement(cktelem);
    DSSPrime.API_VarIdx := elem.LookupVariable(Value);
    if (DSSPrime.API_VarIdx <= 0) or (DSSPrime.API_VarIdx > elem.NumVariables) then
        DoSimpleMsg(DSSPrime, 'Invalid variable name "%s" for "%s"', [Value, elem.FullName], 100001);
end;
//------------------------------------------------------------------------------
function CktElement_Get_VariableValue(): Double; CDECL;
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;

    elem := TPCElement(cktelem);
    if (DSSPrime.API_VarIdx <= 0) or (DSSPrime.API_VarIdx > elem.NumVariables) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid variable index %d for "%s"', [DSSPrime.API_VarIdx, elem.FullName], 100002);
        Exit;
    end;
    Result := elem.Variable[DSSPrime.API_VarIdx];
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_VariableValue(Value: Double); CDECL;
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
begin
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;

    elem := TPCElement(cktelem);
    if (DSSPrime.API_VarIdx <= 0) or (DSSPrime.API_VarIdx > elem.NumVariables) then
    begin
        if DSS_CAPI_EXT_ERRORS then
            DoSimpleMsg(DSSPrime, 'Invalid variable index %d for "%s"', [DSSPrime.API_VarIdx, elem.FullName], 100002);
        Exit;
    end;
    elem.Variable[DSSPrime.API_VarIdx] := Value;
end;
//------------------------------------------------------------------------------
function CktElement_Get_VariableIdx(): Integer; CDECL;
var
    elem: TDSSCktElement;
begin
    Result := -1;
    if InvalidCktElement(DSSPrime, elem, True) then
        Exit;
    Result := DSSPrime.API_VarIdx;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_VariableIdx(Value: Integer); CDECL;
var
    cktelem: TDSSCktElement;
    elem: TPCElement;
begin
    if InvalidCktElement(DSSPrime, cktelem, True) then
    begin
        DSSPrime.API_VarIdx := -1;
        Exit;
    end;

    elem := TPCElement(cktelem);
    if (Value <= 0) or (Value > elem.NumVariables) then
    begin
        if DSS_CAPI_EXT_ERRORS then
            DoSimpleMsg(DSSPrime, 'Invalid variable index %d for "%s"', [DSSPrime.API_VarIdx, elem.FullName], 100003);
        Exit;
    end;
    DSSPrime.API_VarIdx := Value;
end;
//------------------------------------------------------------------------------
function Obj_CktElement_MaxCurrent(obj: TDSSCktElement; terminalIdx: Integer): Double; CDECL;
begin
    Result := Alt_CE_MaxCurrent(obj, terminalIdx);
end;
//------------------------------------------------------------------------------
procedure Obj_Circuit_Set_ActiveCktElement(obj: TDSSCktElement); CDECL;
begin
    obj.DSS.ActiveCircuit.ActiveCktElement := obj;
end;
//------------------------------------------------------------------------------
function CktElement_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ActiveCktElement
end;
//------------------------------------------------------------------------------
end.
