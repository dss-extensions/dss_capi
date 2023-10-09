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

procedure _CalcSeqCurrents(elem: TDSSCktElement; i012: pComplexArray);
// Assumes V012 is properly allocated before call.
var
    i, j, k, iV: Integer;
    IPh, I012a: Complex3;
    cBuffer: pComplexArray;
    DSS: TDSSContext;
begin
    DSS := elem.DSS;
    if elem.NPhases <> 3 then
    begin
        // Handle non-3 phase elements
        if (elem.Nphases = 1) and DSS.ActiveCircuit.PositiveSequence then
        begin
            cBuffer := Allocmem(sizeof(Complex) * elem.NConds * elem.NTerms);
            elem.GetCurrents(cBuffer);

            for i := 1 to 3 * elem.NTerms do
                i012[i] := 0;   // Initialize Result
            iV := 2;  // pos seq is 2nd element in array
            // Populate only phase 1 quantities in Pos seq
            for j := 1 to elem.NTerms do
            begin
                k := (j - 1) * elem.NConds;
                i012[iV] := cBuffer[1 + k];
                Inc(iV, 3);  // inc to pos seq of next terminal
            end;
            Reallocmem(cBuffer, 0);
        end
        // if neither 3-phase or pos seq model, just put in -1.0 for each element
        else
            for i := 1 to 3 * elem.NTerms do
                i012[i] := -1;  // Signify n/A
    end
    else
    begin    // for 3-phase elements
        iV := 1;
        cBuffer := Allocmem(sizeof(Complex) * elem.NConds * elem.NTerms);
        elem.GetCurrents(cBuffer);
        for j := 1 to elem.NTerms do
        begin
            k := (j - 1) * elem.NConds;
            for i := 1 to 3 do
                Iph[i] := cBuffer[k + i];
            Phase2SymComp(@Iph, @I012a);

            for i := 1 to 3 do
            begin     // Stuff it in the result array
                i012[iV] := i012a[i];
                Inc(iV);
            end;
        end;
        Reallocmem(cBuffer, 0);
    end;
end;

//------------------------------------------------------------------------------
procedure CalcSeqVoltages(elem: TDSSCktElement; V012: pComplexArray);
// Assumes V012 is properly allocated before call.
var
    i, j, k, iV: Integer;
    VPh, V012a: Complex3;
    NodeV: pNodeVArray;
    DSS: TDSSContext;
begin
    DSS := elem.DSS;
    NodeV := DSSPrime.ActiveCircuit.Solution.NodeV;

    if elem.NPhases <> 3 then
    begin
        // Handle non-3 phase elements
        if (elem.Nphases = 1) and DSS.ActiveCircuit.PositiveSequence then
        begin
            for i := 1 to 3 * elem.NTerms do
                V012[i] := 0;   // Initialize Result
            iV := 2;  // pos seq is 2nd element in array
            // Populate only phase 1 quantities in Pos seq
            for j := 1 to elem.NTerms do
            begin
                k := (j - 1) * elem.NConds;
                V012[iV] := NodeV[elem.NodeRef[1 + k]];
                Inc(iV, 3);  // inc to pos seq of next terminal
            end;
        end
        // if neither 3-phase or pos seq model, just put in -1.0 for each element
        else
            for i := 1 to 3 * elem.NTerms do
                V012[i] := -1;  // Signify n/A
    end
    else
    begin    // for 3-phase elements
        iV := 1;
        for j := 1 to elem.NTerms do
        begin
            k := (j - 1) * elem.NConds;
            for i := 1 to 3 do
                Vph[i] := NodeV[elem.NodeRef[i + k]];
            Phase2SymComp(@Vph, @V012a);   // Compute Symmetrical components

            for i := 1 to 3 do
            begin     // Stuff it in the result array
                V012[iV] := V012a[i];
                Inc(iV);
            end;
        end;
    end;
end;

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
    Result: PPAnsiCharArray0;
    i: Integer;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;

    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, elem.Nterms);
    for i := 1 to elem.Nterms do
        Result[i - 1] := DSS_CopyStringAsPChar(elem.GetBus(i));

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
    Value: PPAnsiCharArray0;
    i: Integer;
    Count: Integer;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    Value := PPAnsiCharArray0(ValuePtr);
    Count := ValueCount;
    if (Count <> elem.NTerms) AND (DSS_CAPI_EXT_ERRORS) then
    begin
        DoSimpleMsg(DSSPrime, 'The number of buses provided (%d) does not match the number of terminals (%d).', [ValueCount, Integer(elem.NTerms)], 97895);
        Exit;
    end;
    
    if Count > elem.NTerms then
        Count := elem.NTerms;
    for i := 1 to Count do
    begin
        elem.SetBus(i, Value[i - 1]);
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) then
        Exit;
        
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * (elem.NConds * elem.NTerms), elem.NConds, elem.NTerms);
    elem.GetCurrents(pComplexArray(Result));
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
    Result: PDoubleArray0;
    i, n, iV: Integer;
    Volts: Complex;
    elem: TDSSCktElement;
    NodeV: pNodeVArray;
begin
    // Return voltages for all terminals
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) or (elem.NodeRef = NIL) then
        Exit;

    NodeV := DSSPrime.ActiveCircuit.Solution.NodeV;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * (elem.NConds * elem.Nterms), elem.NConds, elem.Nterms);
    // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
    iV := 0;
    for i := 1 to elem.NConds * elem.Nterms do
    begin
        n := elem.NodeRef[i];
        Volts := NodeV[n]; // ok if =0
        Result[iV] := Volts.re;
        Inc(iV);
        Result[iV] := Volts.im;
        Inc(iV);
    end;
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
    Result: PDoubleArray0;
    LossValue: complex;
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    LossValue := elem.Losses;
    Result[0] := LossValue.re;
    Result[1] := LossValue.im;
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
    Result: PDoubleArray0;
    NValues, i: Integer;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    NValues := elem.NPhases;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    elem.GetPhaseLosses(NValues, pComplexArray(Result));
    for i := 0 to (2 * NValues - 1) do
    begin
        Result[i] *= 0.001;
    end;
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
    Result: PDoubleArray0;
    NValues,
    i: Integer;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    NValues := elem.NConds * elem.Nterms;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues, elem.NConds, elem.NTerms);
    elem.GetPhasePower(pComplexArray(ResultPtr));
    for i := 0 to (2 * NValues - 1) do
        Result[i] *= 0.001;
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
    Result: PDoubleArray0;
    i: Integer;
    i012: pComplexArray;
    S: String;
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) or (not elem.Enabled) then
        Exit;

    try
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 3 * elem.NTerms, 3, elem.NTerms);

        i012 := Allocmem(sizeof(Complex) * 3 * elem.Nterms);
        // get complex seq voltages
        _CalcSeqCurrents(elem, i012);
        // return 0 based array
        for i := 1 to 3 * elem.Nterms do
            Result[i - 1] := Cabs(i012[i]);  // return mag only

        Reallocmem(i012, 0);  // throw away temp memory

    except
        On E: Exception do
        begin
            S := E.message + CRLF +
                'Element=' + elem.Name + CRLF +
                'Nphases=' + IntToStr(elem.Nphases) + CRLF +
                'NTerms=' + IntToStr(elem.NTerms) + CRLF +
                'NConds =' + IntToStr(elem.NConds);
            DoSimpleMsg(DSSPrime, S, 5012);
        end;
    end;
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
    Result: PDoubleArray0;
    Nvalues, i, j, k, n, icount: Integer;
    S: Complex;
    VPh, V012: Complex3;
    IPh, I012: Complex3;
    cBuffer: pComplexArray;

    elem: TDSSCktElement;
    NodeV: pNodeVArray;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) or (elem.NodeRef = NIL) then // or (not elem.Enabled)
        Exit;

    NodeV := DSSPrime.ActiveCircuit.Solution.NodeV;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * elem.NTerms, 3, elem.NTerms); // allocate for kW and kvar
    if elem.NPhases <> 3 then
    begin
        if (elem.Nphases = 1) and DSSPrime.ActiveCircuit.PositiveSequence then
        begin
            NValues := elem.NConds * elem.NTerms;
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            elem.GetCurrents(cBuffer);
            iCount := 2;  // Start with kW1
            // Put only phase 1 quantities in Pos seq
            for j := 1 to elem.NTerms do
            begin
                k := (j - 1) * elem.NConds;
                n := elem.NodeRef[k + 1];
                Vph[1] := NodeV[n];  // Get voltage at node
                S := Vph[1] * cong(cBuffer[k + 1]);   // Compute power per phase
                Result[icount] := S.re * 0.003; // 3-phase kW conversion
                inc(icount);
                Result[icount] := S.im * 0.003; // 3-phase kvar conversion
                inc(icount, 5);
            end;
            Reallocmem(cBuffer, 0);
        end
        else
            for i := 0 to 2 * 3 * elem.NTerms - 1 do
                Result[i] := -1.0;  // Signify n/A
    end
    else
    begin
        NValues := elem.NConds * elem.NTerms;
        cBuffer := Allocmem(sizeof(Complex) * NValues);
        elem.GetCurrents(cBuffer);
        icount := 0;
        for j := 1 to elem.NTerms do
        begin
            k := (j - 1) * elem.NConds;
            for i := 1 to 3 do
                Vph[i] := NodeV[elem.NodeRef[i + k]];
            for i := 1 to 3 do
                Iph[i] := cBuffer[k + i];
            Phase2SymComp(@Iph, @I012);
            Phase2SymComp(@Vph, @V012);
            for i := 1 to 3 do
            begin
                S := V012[i] * cong(I012[i]);
                Result[icount] := S.re * 0.003; // 3-phase kW conversion
                inc(icount);
                Result[icount] := S.im * 0.003; // 3-phase kvar conversion
                inc(icount);
            end;
        end;
        Reallocmem(cBuffer, 0);
    end;
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
    
    if InvalidCktElement(DSSPrime, elem) 
        or MissingSolution(DSSPrime) 
        or (not elem.Enabled)
        or (elem.NodeRef = NIL) 
    then
        Exit;

    try
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 3 * elem.NTerms, 3, elem.NTerms);

        V012 := Allocmem(sizeof(Complex) * 3 * elem.Nterms);
        // get complex seq voltages
        CalcSeqVoltages(elem, V012);
        // return 0 based array
        for i := 1 to 3 * elem.Nterms do
            Result[i - 1] := Cabs(V012[i]);  // return mag only

        Reallocmem(V012, 0);  // throw away temp memory

    except
        On E: Exception do
        begin
            S := E.message + CRLF +
                'Element=' + elem.Name + CRLF +
                'Nphases=' + IntToStr(elem.Nphases) + CRLF +
                'NTerms=' + IntToStr(elem.NTerms) + CRLF +
                'NConds =' + IntToStr(elem.NConds);
            DoSimpleMsg(DSSPrime, S, 5012);
        end;
    end;
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

    //TODO: why is this changing ActiveTerminal directly?
    elem.ActiveTerminal := @elem.Terminals[Term - 1];
    elem.Closed[Phs] := TRUE;
end;
//------------------------------------------------------------------------------
procedure CktElement_Open(Term, Phs: Integer); CDECL;
var
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) then
        Exit;

    //TODO: why is this changing ActiveTerminal directly?
    elem.ActiveTerminal := @elem.Terminals[Term - 1];
    elem.Closed[Phs] := FALSE;
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
    i: Integer;
    elem: TDSSCktElement;
begin
    Result := False;
    if InvalidCktElement(DSSPrime, elem) then
        Exit;
    
    //TODO: why is this changing ActiveTerminal directly?
    elem.ActiveTerminal := @elem.Terminals[Term - 1];
    if Phs = 0 then // At least one must be open
    begin
        Result := FALSE;
        for i := 1 to elem.NConds do
            if not elem.Closed[i] then
            begin
                Result := TRUE;
                Exit;
            end;
    end
    else // Check a specific phase or conductor
        Result := not elem.Closed[Phs];
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
    S: String;
    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);

    if InvalidCktElement(DSSPrime, elem) 
        or MissingSolution(DSSPrime) 
        or (not elem.Enabled) 
        or (elem.NodeRef = NIL)
     then
        Exit;

    try
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * elem.NTerms, 3, elem.NTerms);
        CalcSeqVoltages(elem, pComplexArray(ResultPtr));

    except
        On E: Exception do
        begin
            S := E.message + CRLF +
                'Element=' + elem.Name + CRLF +
                'Nphases=' + IntToStr(elem.Nphases) + CRLF +
                'NTerms=' + IntToStr(elem.NTerms) + CRLF +
                'NConds =' + IntToStr(elem.NConds);
            DoSimpleMsg(DSSPrime, S, 5012);
        end;
    end;
end;

procedure CktElement_Get_CplxSeqVoltages_GR(); CDECL;
// Same as CktElement_Get_CplxSeqVoltages but uses global result (GR) pointers
begin
    CktElement_Get_CplxSeqVoltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_CplxSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    i012: pComplexArray;
    S: String;

    elem: TDSSCktElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) or (not elem.Enabled) then
        Exit;

    try
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * elem.NTerms, 3, elem.NTerms);
        i012 := pComplexArray(Result);
        // get complex seq voltages
        _CalcSeqCurrents(elem, i012);

    except
        On E: Exception do
        begin
            S := E.message + CRLF +
                'Element=' + elem.Name + CRLF +
                'Nphases=' + IntToStr(elem.Nphases) + CRLF +
                'NTerms=' + IntToStr(elem.NTerms) + CRLF +
                'NConds =' + IntToStr(elem.NConds);
            DoSimpleMsg(DSSPrime, S, 5012);
        end;
    end;
end;

procedure CktElement_Get_CplxSeqCurrents_GR(); CDECL;
// Same as CktElement_Get_CplxSeqCurrents but uses global result (GR) pointers
begin
    CktElement_Get_CplxSeqCurrents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    cktelem: TDSSCktElement;
    Result: PPAnsiCharArray0;
    k: Integer;
    elem: TPCElement;
begin
    DefaultResult(ResultPtr, ResultCount, '');
    
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;

    elem := (cktelem as TPCElement);
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, elem.NumVariables);
    for k := 1 to elem.NumVariables do
    begin
        Result[k - 1] := DSS_CopyStringAsPChar(elem.VariableName(k));
    end;
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
    cktelem: TDSSCktElement;
    Result: PDoubleArray0;
    k: Integer;
    elem: TPCElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, cktelem, True) then
        Exit;
    
    elem := (cktelem as TPCElement);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NumVariables);
    for k := 1 to elem.NumVariables do
    begin
        Result[k - 1] := elem.Variable[k];
    end;
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
    Result: PDoubleArray0;
    cBuffer: pComplexArray;
    CMagAng: polar;
    NValues, iV, i: Integer;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    NValues := elem.NConds * elem.NTerms;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues, 2, NValues);
    cBuffer := PComplexArray(ResultPtr);
    elem.GetCurrents(cBuffer);
    iV := 0;
    for i := 1 to NValues do
    begin
        CMagAng := ctopolardeg(cBuffer[i]); // convert to mag/angle
        Result[iV] := CMagAng.mag;
        Inc(iV);
        Result[iV] := CMagAng.ang;
        Inc(iV);
    end;
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
    Result: PDoubleArray0;
    numcond, i, n, iV: Integer;
    Volts: Polar;
    NodeV: pNodeVArray;
    elem: TDSSCktElement;
begin
    // Return voltages for all terminals
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) or (elem.NodeRef = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    NodeV := DSSPrime.ActiveCircuit.Solution.NodeV;

    numcond := elem.NConds * elem.Nterms;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * numcond, 2, numcond);
    // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
    iV := 0;
    for i := 1 to numcond do
    begin
        n := elem.NodeRef[i];
        Volts := ctopolardeg(NodeV[n]); // ok if =0
        Result[iV] := Volts.mag;
        Inc(iV);
        Result[iV] := Volts.ang;
        Inc(iV);
    end;
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
    cBuffer: pComplexArray;
    myInit,
    myEnd,
    j,
    i,
    iV: Integer;
    buffer: Array of Complex;
    Result: PDoubleArray0;
    elem: TDSSCktElement;
begin
    if InvalidCktElement(DSSPrime, elem) or MissingSolution(DSSPrime) or (elem.NodeRef = NIL) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * elem.Nterms);
    cBuffer := Allocmem(2 * SizeOf(Double) * elem.NConds * elem.Nterms);
    elem.GetPhasePower(cBuffer);
    iV := 0;
    SetLength(buffer, elem.Nterms);
    for j := 1 to elem.Nterms do
    Begin
        buffer[j - 1] := 0;
        myInit := (j - 1) * elem.NConds + 1;
        myEnd := elem.NConds * j;
        for i := myInit to myEnd do
        begin
            buffer[j - 1] := buffer[j - 1] + cBuffer[i];
        end;
        Result[iV + 0] := buffer[j - 1].re * 0.001;
        Result[iV + 1] := buffer[j - 1].im * 0.001; 
        Inc(iV, 2);
    End;
    Reallocmem(cBuffer,0);
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
    Result := obj.MaxCurrent[terminalIdx];
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
