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
function Obj_CktElement_MaxCurrent(obj: TDSSCktElement; terminalIdx: Integer): Double;
procedure Obj_Circuit_Set_ActiveCktElement(obj: TDSSCktElement);

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
    DSSHelper;

procedure _CalcSeqCurrents(pActiveElement: TDSSCktElement; i012: pComplexArray);
// Assumes V012 is properly allocated before call.
var
    Nvalues, i, j, k, iV: Integer;
    IPh, I012a: Complex3;
    cBuffer: pComplexArray;
begin
    with pActiveElement, DSSPrime.ActiveCircuit do
    begin
        Nvalues := NPhases;
        if Nvalues <> 3 then
        begin
            // Handle non-3 phase elements
            if (Nphases = 1) and PositiveSequence then
            begin
                NValues := NConds * NTerms;
                cBuffer := Allocmem(sizeof(Complex) * NValues);
                GetCurrents(cBuffer);

                for i := 1 to 3 * NTerms do
                    i012[i] := 0;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                // Populate only phase 1 quantities in Pos seq
                for j := 1 to NTerms do
                begin
                    k := (j - 1) * NConds;
                    i012[iV] := cBuffer[1 + k];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                end;
                Reallocmem(cBuffer, 0);
            end
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
            else
                for i := 1 to 3 * NTerms do
                    i012[i] := -1;  // Signify n/A
        end
        else
        begin    // for 3-phase elements
            iV := 1;
            NValues := NConds * NTerms;
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetCurrents(cBuffer);
            for j := 1 to NTerms do
            begin
                k := (j - 1) * NConds;
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
end;

//------------------------------------------------------------------------------
procedure CalcSeqVoltages(pActiveElement: TDSSCktElement; V012: pComplexArray);
{Assumes V012 is properly allocated before call.}
var
    Nvalues, i, j, k, iV: Integer;
    VPh, V012a: Complex3;
begin
    with pActiveElement, DSSPrime.ActiveCircuit do
    begin
        Nvalues := NPhases;
        if Nvalues <> 3 then
        begin
        {Handle non-3 phase elements}
            if (Nphases = 1) and PositiveSequence then
            begin
                for i := 1 to 3 * NTerms do
                    V012[i] := 0;   // Initialize Result
                iV := 2;  // pos seq is 2nd element in array
                {Populate only phase 1 quantities in Pos seq}
                for j := 1 to NTerms do
                begin
                    k := (j - 1) * NConds;
                    V012[iV] := Solution.NodeV[NodeRef[1 + k]];
                    Inc(iV, 3);  // inc to pos seq of next terminal
                end;
            end
           // if neither 3-phase or pos seq model, just put in -1.0 for each element
            else
                for i := 1 to 3 * NTerms do
                    V012[i] := -1;  // Signify n/A
        end
        else
        begin    // for 3-phase elements
            iV := 1;
            for j := 1 to NTerms do
            begin
                k := (j - 1) * NConds;
                for i := 1 to 3 do
                    Vph[i] := Solution.NodeV[NodeRef[i + k]];
                Phase2SymComp(@Vph, @V012a);   // Compute Symmetrical components

                for i := 1 to 3 do
                begin     // Stuff it in the result array
                    V012[iV] := V012a[i];
                    Inc(iV);
                end;
            end;
        end;
    end;
end;

//------------------------------------------------------------------------------
function IsPDElement(DSS: TDSSContext): Boolean;
begin
    Result := ((DSS.ActiveCircuit.ActiveCktElement.DSSObjType and 3) = PD_ELEMENT)
end;
//------------------------------------------------------------------------------
function InvalidCktElement(DSS: TDSSContext; const NeedsPCElement: Boolean=False): Boolean; inline;
var
    elem: TDSSCktElement;
begin
    Result := InvalidCircuit(DSS);
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
begin
    if InvalidCktElement(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, Nterms);
        for i := 1 to Nterms do
            Result[i - 1] := DSS_CopyStringAsPChar(GetBus(i));
    end;

end;

procedure CktElement_Get_BusNames_GR(); CDECL;
// Same as CktElement_Get_BusNames but uses global result (GR) pointers
begin
    CktElement_Get_BusNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_Name(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
        Result := DSS_GetAsPAnsiChar(DSSPrime, FullName);
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumConductors(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime) then
        Exit;

    Result := DSSPrime.ActiveCircuit.ActiveCktElement.NConds
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumPhases(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime) then
        Exit;

    Result := DSSPrime.ActiveCircuit.ActiveCktElement.NPhases
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumTerminals(): Integer; CDECL;
begin
    if InvalidCktElement(DSSPrime) then
    begin
        Result := 0;
        Exit;
    end;

    Result := DSSPrime.ActiveCircuit.ActiveCktElement.NTerms
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_BusNames(ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
var
    Value: PPAnsiCharArray0;
    i: Integer;
    Count: Integer;
begin
    if InvalidCktElement(DSSPrime) then
        Exit;

    Value := PPAnsiCharArray0(ValuePtr);
    with DSSPrime.ActiveCircuit do
    begin
        Count := ValueCount;
        if (Count <> ActiveCktElement.NTerms) AND (DSS_CAPI_EXT_ERRORS) then
        begin
            DoSimpleMsg(DSSPrime, 
                'The number of buses provided (%d) does not match the number of terminals (%d).', 
                    [ValueCount, Integer(ActiveCktElement.NTerms)]
                , 97895
            );
            Exit;
        end;
        
        if Count > ActiveCktElement.NTerms then
            Count := ActiveCktElement.NTerms;
        for i := 1 to Count do
        begin
            ActiveCktElement.SetBus(i, Value[i - 1]);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) then
        Exit;
        
    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * (NConds * NTerms), NConds, NTerms);
        GetCurrents(pComplexArray(Result));
    end
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
begin
    // Return voltages for all terminals
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) or (DSSPrime.ActiveCircuit.ActiveCktElement.NodeRef = NIL) then
        Exit;

    with DSSPrime.ActiveCircuit, ActiveCktElement do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * (NConds * Nterms), NConds, Nterms);
        // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
        iV := 0;
        for i := 1 to NConds * Nterms do
        begin
            n := ActiveCktElement.NodeRef[i];
            Volts := Solution.NodeV[n]; // ok if =0
            Result[iV] := Volts.re;
            Inc(iV);
            Result[iV] := Volts.im;
            Inc(iV);
        end;
    end;
end;

procedure CktElement_Get_Voltages_GR(); CDECL;
// Same as CktElement_Get_Voltages but uses global result (GR) pointers
begin
    CktElement_Get_Voltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_EmergAmps(): Double; CDECL;
begin
    Result := 0;
    
    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit do
        if (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT then
            with ActiveCktElement as TPDElement do
                Result := EmergAmps;
end;

//------------------------------------------------------------------------------
function CktElement_Get_Enabled(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    
    if InvalidCktElement(DSSPrime) then
        Exit;

    Result := DSSPrime.ActiveCircuit.ActiveCktElement.Enabled
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_Losses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    LossValue: complex;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        LossValue := ActiveCktElement.Losses;
        Result[0] := LossValue.re;
        Result[1] := LossValue.im;
    end;
end;


procedure CktElement_Get_Losses_GR(); CDECL;
// Same as CktElement_Get_Losses but uses global result (GR) pointers
begin
    CktElement_Get_Losses(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_NormalAmps(): Double; CDECL;
begin
    Result := 0;
    
    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit do
        if (ActiveCktElement.DSSObjType and 3) = PD_ELEMENT then
            with ActiveCktElement as TPDElement do
                Result := NormAmps;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_PhaseLosses(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Returns Phase losses in kW, kVar
var
    Result: PDoubleArray0;
    NValues, i: Integer;
begin
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        NValues := NPhases;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
        GetPhaseLosses(NValues, pComplexArray(Result));
        for i := 0 to (2 * NValues - 1) do
        begin
            Result[i] *= 0.001;
        end;
    end
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
begin
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        NValues := NConds * Nterms;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
        GetPhasePower(pComplexArray(ResultPtr));
        for i := 0 to (2 * NValues - 1) do
            Result[i] *= 0.001;
    end
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
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) or (not DSSPrime.ActiveCircuit.ActiveCktElement.Enabled) then
        Exit;

    with DSSPrime.ActiveCircuit, ActiveCktElement do
    begin
        try
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 3 * NTerms, 3, NTerms);

            i012 := Allocmem(sizeof(Complex) * 3 * Nterms);
            // get complex seq voltages
            _CalcSeqCurrents(ActiveCktElement, i012);
            // return 0 based array
            for i := 1 to 3 * Nterms do
                Result[i - 1] := Cabs(i012[i]);  // return mag only

            Reallocmem(i012, 0);  // throw away temp memory

        except
            On E: Exception do
            begin
                S := E.message + CRLF +
                    'Element=' + ActiveCktElement.Name + CRLF +
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
            end;
        end;
    end
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

begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) or (DSSPrime.ActiveCircuit.ActiveCktElement.NodeRef = NIL) {or (not DSSPrime.ActiveCircuit.ActiveCktElement.Enabled)} then
        Exit;

    with DSSPrime.ActiveCircuit, ActiveCktElement do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * NTerms, 3, NTerms); // allocate for kW and kvar
        if NPhases <> 3 then
        begin
            if (Nphases = 1) and PositiveSequence then
            begin
                NValues := NConds * NTerms;
                cBuffer := Allocmem(sizeof(Complex) * NValues);
                GetCurrents(cBuffer);
                iCount := 2;  // Start with kW1
                // Put only phase 1 quantities in Pos seq
                for j := 1 to NTerms do
                begin
                    k := (j - 1) * NConds;
                    n := NodeRef[k + 1];
                    Vph[1] := Solution.NodeV[n];  // Get voltage at node
                    S := Vph[1] * cong(cBuffer[k + 1]);   // Compute power per phase
                    Result[icount] := S.re * 0.003; // 3-phase kW conversion
                    inc(icount);
                    Result[icount] := S.im * 0.003; // 3-phase kvar conversion
                    inc(icount, 5);
                end;
                Reallocmem(cBuffer, 0);
            end
            else
                for i := 0 to 2 * 3 * NTerms - 1 do
                    Result[i] := -1.0;  // Signify n/A
        end
        else
        begin
            NValues := NConds * NTerms;
            cBuffer := Allocmem(sizeof(Complex) * NValues);
            GetCurrents(cBuffer);
            icount := 0;
            for j := 1 to NTerms do
            begin
                k := (j - 1) * NConds;
                for i := 1 to 3 do
                    Vph[i] := Solution.NodeV[NodeRef[i + k]];
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
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) 
        or MissingSolution(DSSPrime) 
        or (not DSSPrime.ActiveCircuit.ActiveCktElement.Enabled)  
        or (DSSPrime.ActiveCircuit.ActiveCktElement.NodeRef = NIL) 
    then
        Exit;

    with DSSPrime.ActiveCircuit, ActiveCktElement do
    begin
        try
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 3 * NTerms, 3, NTerms);

            V012 := Allocmem(sizeof(Complex) * 3 * Nterms);
            // get complex seq voltages
            CalcSeqVoltages(ActiveCktElement, V012);
            // return 0 based array
            for i := 1 to 3 * Nterms do
                Result[i - 1] := Cabs(V012[i]);  // return mag only

            Reallocmem(V012, 0);  // throw away temp memory

        except
            On E: Exception do
            begin
                S := E.message + CRLF +
                    'Element=' + ActiveCktElement.Name + CRLF +
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
            end;
        end;
    end
end;

procedure CktElement_Get_SeqVoltages_GR(); CDECL;
// Same as CktElement_Get_SeqVoltages but uses global result (GR) pointers
begin
    CktElement_Get_SeqVoltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Close(Term, Phs: Integer); CDECL;
begin
    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        //TODO: why is this changing ActiveTerminal directly?
        ActiveTerminal := @Terminals[Term - 1];
        Closed[Phs] := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Open(Term, Phs: Integer); CDECL;
begin
    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        //TODO: why is this changing ActiveTerminal directly?
        ActiveTerminal := @Terminals[Term - 1];
        Closed[Phs] := FALSE;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_EmergAmps(Value: Double); CDECL;
begin
    if InvalidCktElement(DSSPrime) then
        Exit;

    if IsPDElement(DSSPrime) then
        with DSSPrime.ActiveCircuit.ActiveCktElement as TPDElement do
            EmergAmps := Value;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_Enabled(Value: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ActiveCktElement.Enabled := Value;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_NormalAmps(Value: Double); CDECL;
begin
    if InvalidCktElement(DSSPrime) then
        Exit;

    if IsPDElement(DSSPrime) then
        with DSSPrime.ActiveCircuit.ActiveCktElement as TPDElement do
            NormAmps := Value;
end;
//------------------------------------------------------------------------------
function CktElement_IsOpen(Term, Phs: Integer): TAPIBoolean; CDECL;
var
    i: Integer;
begin
    Result := False;
    if InvalidCktElement(DSSPrime) then
        Exit;
    
    with DSSPrime.ActiveCircuit do
    begin
        with ActiveCktElement do
            //TODO: why is this changing ActiveTerminal directly?
            ActiveTerminal := @Terminals[Term - 1];
        if Phs = 0 then // At least one must be open
        begin
            Result := FALSE;
            for i := 1 to ActiveCktElement.NConds do
                if not ActiveCktElement.Closed[i] then
                begin
                    Result := TRUE;
                    Exit;
                end;
        end
        else // Check a specific phase or conductor
            Result := not ActiveCktElement.Closed[Phs];
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_AllPropertyNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
begin
    if InvalidCktElement(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;
    
    with DSSPrime.ActiveCircuit.ActiveCktElement, ParentClass do
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumProperties);
        for k := 1 to NumProperties do
        begin
            Result[k - 1] := DSS_CopyStringAsPChar(PropertyName[k]);
        end;
    end;
end;

procedure CktElement_Get_AllPropertyNames_GR(); CDECL;
// Same as CktElement_Get_AllPropertyNames but uses global result (GR) pointers
begin
    CktElement_Get_AllPropertyNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_NumProperties(): Integer; CDECL;
begin
    Result := 0;
    
    if InvalidCktElement(DSSPrime) then
        Exit;
    
    Result := DSSPrime.ActiveCircuit.ActiveCktElement.ParentClass.NumProperties;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_Residuals(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    cBuffer: pComplexArray;
    iV, i, j, k: Integer;
    cResid: Complex;

begin
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NTerms, 2, NTerms);    // 2 values per terminal
        cBuffer := Allocmem(sizeof(Complex) * Yorder);
        GetCurrents(cBuffer);
        iV := 0;
        for i := 1 to NTerms do
        begin
            cResid := 0;
            k := (i - 1) * Nconds;
            for j := 1 to Nconds do
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
    end
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
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
        if cValues = NIL then 
            Exit;

        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * Yorder * Yorder, Yorder, Yorder);
        Move(cValues^, ResultPtr^, ResultCount^ * SizeOf(Double));
    end
end;

procedure CktElement_Get_Yprim_GR(); CDECL;
// Same as CktElement_Get_Yprim but uses global result (GR) pointers
begin
    CktElement_Get_Yprim(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function CktElement_Get_DisplayName(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime) then
        Exit;

    if DSSPrime.ActiveCircuit.ActiveCktElement.DisplayName <> '' then
        Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveCircuit.ActiveCktElement.DisplayName)
    else
        Result := DSS_GetAsPAnsiChar(DSSPrime, 
            DSSPrime.ActiveCircuit.ActiveCktElement.ParentClass.Name + '_' +
            DSSPrime.ActiveCircuit.ActiveCktElement.Name
        );
end;

//------------------------------------------------------------------------------
function CktElement_Get_GUID(): PAnsiChar; CDECL;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, DSSPrime.ActiveCircuit.ActiveCktElement.ID)
end;

//------------------------------------------------------------------------------
function CktElement_Get_Handle(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ActiveCktElement.Handle
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_DisplayName(const Value: PAnsiChar); CDECL;
begin
    if InvalidCktElement(DSSPrime) then
        Exit;

    DSSPrime.ActiveCircuit.ActiveCktElement.DisplayName := Value;
end;
//------------------------------------------------------------------------------
function CktElement_Get_Controller(idx: Integer): PAnsiChar; CDECL;
var
    ctrl: TDSSCktElement;
begin
    Result := NIL;

    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit do
    begin
        if (idx > 0) and (idx <= ActiveCktElement.ControlElementList.Count) then
        begin
            ctrl := ActiveCktElement.ControlElementList.Get(idx);
            if ctrl <> NIL then
                Result := DSS_GetAsPAnsiChar(DSSPrime, ctrl.FullName);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_EnergyMeter(): PAnsiChar; CDECL;
var
    pd: TPDElement;
begin
    Result := NIL;

    if InvalidCktElement(DSSPrime) then
        Exit;

    if Flg.HasEnergyMeter in DSSPrime.ActiveCircuit.ActiveCktElement.Flags then
    begin
        pd := DSSPrime.ActiveCircuit.ActiveCktElement as TPDElement;
        Result := DSS_GetAsPAnsiChar(DSSPrime, pd.MeterObj.Name);
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasVoltControl(): TAPIBoolean; CDECL;
// Returns true if any of the controls is a capcontrol or a regcontrol
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;

    if InvalidCktElement(DSSPrime) then
        Exit;

    for ctrl in DSSPrime.ActiveCircuit.ActiveCktElement.ControlElementlist do
    begin
        case (ctrl.DSSObjType and CLASSMASK) of
            CAP_CONTROL,
            REG_CONTROL:
                Result := TRUE;
        else
            Result := FALSE;
        end;
        if Result then
            Exit;
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_HasSwitchControl(): TAPIBoolean; CDECL;
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;

    if InvalidCktElement(DSSPrime) then
        Exit;

    for ctrl in DSSPrime.ActiveCircuit.ActiveCktElement.ControlElementList do
    begin
        case (ctrl.DSSObjType and CLASSMASK) of
            SWT_CONTROL:
                Result := TRUE;
        else
            Result := FALSE;
        end;
        if Result then
            Exit;
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    S: String;
begin
    DefaultResult(ResultPtr, ResultCount);

    if InvalidCktElement(DSSPrime) 
        or MissingSolution(DSSPrime) 
        or (not DSSPrime.ActiveCircuit.ActiveCktElement.Enabled) 
        or (DSSPrime.ActiveCircuit.ActiveCktElement.NodeRef = NIL)
     then
        Exit;

    with DSSPrime.ActiveCircuit, ActiveCktElement do
    begin
        try
            DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * NTerms, 3, NTerms);
            CalcSeqVoltages(ActiveCktElement, pComplexArray(ResultPtr));

        except
            On E: Exception do
            begin
                S := E.message + CRLF +
                    'Element=' + ActiveCktElement.Name + CRLF +
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
            end;
        end;
    end
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

begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) or (not DSSPrime.ActiveCircuit.ActiveCktElement.Enabled) then
        Exit;

    with DSSPrime.ActiveCircuit, ActiveCktElement do
    begin
        try
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * NTerms, 3, NTerms);
            i012 := pComplexArray(Result);
            // get complex seq voltages
            _CalcSeqCurrents(ActiveCktElement, i012);

        except
            On E: Exception do
            begin
                S := E.message + CRLF +
                    'Element=' + ActiveCktElement.Name + CRLF +
                    'Nphases=' + IntToStr(Nphases) + CRLF +
                    'NTerms=' + IntToStr(NTerms) + CRLF +
                    'NConds =' + IntToStr(NConds);
                DoSimpleMsg(S, 5012);
            end;
        end;
    end
end;

procedure CktElement_Get_CplxSeqCurrents_GR(); CDECL;
// Same as CktElement_Get_CplxSeqCurrents but uses global result (GR) pointers
begin
    CktElement_Get_CplxSeqCurrents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure CktElement_Get_AllVariableNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
    elem: TPCElement;
begin
    DefaultResult(ResultPtr, ResultCount, '');
    
    if InvalidCktElement(DSSPrime, True) then
        Exit;

    elem := (DSSPrime.ActiveCircuit.ActiveCktElement as TPCElement);
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
{Return array of doubles with values of all variables if PCElement}
var
    Result: PDoubleArray0;
    k: Integer;
    elem: TPCElement;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if InvalidCktElement(DSSPrime, True) then
        Exit;
    
    elem := (DSSPrime.ActiveCircuit.ActiveCktElement as TPCElement);
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
    elem: TPCElement;
    VarIndex: Integer;
begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, True) then
        Exit;
    
    elem := (DSSPrime.ActiveCircuit.ActiveCktElement as TPCElement);
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
    elem: TPCElement;
begin
    Result := 0.0;
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, True) then
        Exit;
    
    elem := (DSSPrime.ActiveCircuit.ActiveCktElement as TPCElement);
    if (Idx > 0) and (Idx <= elem.NumVariables) then
    begin
        Result := elem.Variable[Idx];
        Code := 0;  // Signify result is OK.
    end;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_Variable(const VarName: PAnsiChar; out Code: Integer; Value: Double); CDECL; //TODO: Remove Code and use Error interface?
var
    elem: TPCElement;
    VarIndex: Integer;
begin
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, True) then
        Exit;
    
    elem := (DSSPrime.ActiveCircuit.ActiveCktElement as TPCElement);
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
    elem: TPCElement;
begin
    Code := 1; // Signifies an error; no value set
    
    if InvalidCktElement(DSSPrime, True) then
        Exit;
    
    elem := (DSSPrime.ActiveCircuit.ActiveCktElement as TPCElement);
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
begin
    if InvalidCktElement(DSSPrime) then
    begin
        // Just ignore as the original code did
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        if NodeRef = NIL then
        begin
            // Warn and exit
            DoSimpleMsg(_('Nodes are not initialized. Try solving the system first.'), 15013);
            DefaultResult(ResultPtr, ResultCount);
            Exit;
        end;

        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, NTerms * Nconds, Nconds, NTerms);
        k := 0;
        for i := 1 to Nterms do
        begin
            for j := (i - 1) * NConds + 1 to i * Nconds do
            begin
                Result[k] := GetNodeNum(DSSPrime, NodeRef[j]);
                inc(k);
            end;
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
begin
    if InvalidCktElement(DSSPrime) then
    begin
        Result := FALSE;
        Exit;
    end;
    Result := Flg.HasOCPDevice in DSSPrime.ActiveCircuit.ActiveCktElement.Flags;
end;
//------------------------------------------------------------------------------
function CktElement_Get_NumControls(): Integer; CDECL;
begin
    if InvalidCktElement(DSSPrime) then
    begin
        Result := 0;
        Exit;
    end;
    Result := DSSPrime.ActiveCircuit.ActiveCktElement.ControlElementList.Count;
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevIndex(): Integer; CDECL;
var
    iControl: Integer;
    pCktElement: TDSSCktElement;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime) then
        Exit;

    with DSSPrime.ActiveCircuit do
    begin
        iControl := 1;
        repeat
            // cycle through the list of controls until we find a fuse, recloser, or relay
            pCktElement := ActiveCktElement.ControlElementList.Get(iControl);
            if pCktElement <> NIL then
                case (pCktElement.DSSObjType and CLASSMASK) of
                    FUSE_CONTROL:
                        Result := iControl;
                    RECLOSER_CONTROL:
                        Result := iControl;
                    RELAY_CONTROL:
                        Result := iControl;
                end;
            inc(iControl);
        until (iControl > ActiveCktElement.ControlElementList.Count) or (Result > 0);
    end;
end;
//------------------------------------------------------------------------------
function CktElement_Get_OCPDevType(): Integer; CDECL;
begin
    if InvalidCktElement(DSSPrime) then
    begin
        Result := 0;
        Exit;
    end;
    Result := GetOCPDeviceType(DSSPrime.ActiveCircuit.ActiveCktElement);     // see Utilities.pas
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// return currents in magnitude, angle array
var
    Result: PDoubleArray0;
    cBuffer: pComplexArray;
    CMagAng: polar;
    NValues, iV, i: Integer;
begin
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        NValues := NConds * NTerms;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues, 2, NValues);
        cBuffer := PComplexArray(ResultPtr);
        GetCurrents(cBuffer);
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
begin
    // Return voltages for all terminals
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) or (DSSPrime.ActiveCircuit.ActiveCktElement.NodeRef = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with DSSPrime.ActiveCircuit, ActiveCktElement do
    begin
        numcond := NConds * Nterms;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * numcond, 2, numcond);
        // k := (Terminal-1)*numcond;    // RCD 8-30-00 Changed
        iV := 0;
        for i := 1 to numcond do
        begin
            n := ActiveCktElement.NodeRef[i];
            Volts := ctopolardeg(Solution.NodeV[n]); // ok if =0
            Result[iV] := Volts.mag;
            Inc(iV);
            Result[iV] := Volts.ang;
            Inc(iV);
        end;
    end;
end;

procedure CktElement_Get_VoltagesMagAng_GR(); CDECL;
// Same as CktElement_Get_VoltagesMagAng but uses global result (GR) pointers
begin
    CktElement_Get_VoltagesMagAng(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
function CktElement_Get_IsIsolated(): TAPIBoolean; CDECL;
begin
    if InvalidCktElement(DSSPrime) then
    begin
        Result := FALSE;
        Exit;
    end;

    Result := Flg.IsIsolated in DSSPrime.ActiveCircuit.ActiveCktElement.Flags;
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
begin
    if InvalidCktElement(DSSPrime) or MissingSolution(DSSPrime) or (DSSPrime.ActiveCircuit.ActiveCktElement.NodeRef = NIL) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Exit;
    end;

    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * Nterms);
        cBuffer := Allocmem(2 * SizeOf(Double) * NConds * Nterms);
        GetPhasePower(cBuffer);
        iV := 0;
        SetLength(buffer, Nterms);
        for j := 1 to Nterms do
        Begin
            buffer[j - 1] := 0;
            myInit := (j - 1) * NConds + 1;
            myEnd := NConds * j;
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
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_TotalPowers_GR(); CDECL;
// Same as CktElement_Get_TotalPowers but uses global result (GR) pointers
begin
    CktElement_Get_TotalPowers(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
procedure CktElement_Get_NodeRef(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;    
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCktElement(DSSPrime) then 
        Exit;
        
    if DSSPrime.ActiveCircuit.ActiveCktElement.NodeRef = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, _('NodeRef is not populated for the current element!'), 97801);
        end;
        Exit;
    end;
    with DSSPrime.ActiveCircuit.ActiveCktElement do
    begin
        DSS_RecreateArray_PInteger(ResultPtr, ResultCount, Yorder);
        Move(NodeRef[1], ResultPtr^, Yorder * SizeOf(Integer));
    end;
end;

procedure CktElement_Get_NodeRef_GR(); CDECL;
// Same as CktElement_Get_NodeRef but uses global result (GR) pointers
begin
    CktElement_Get_NodeRef(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;
//------------------------------------------------------------------------------
function CktElement_Get_VariableName(): PAnsiChar; CDECL;
var
    elem: TPCElement;
begin
    Result := NIL;
    if InvalidCktElement(DSSPrime, True) then
        Exit;

    elem := TPCElement(DSSPrime.ActiveCircuit.ActiveCktElement);
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
    elem: TPCElement;
begin
    if InvalidCktElement(DSSPrime, True) then
    begin
        DSSPrime.API_VarIdx := -1;
        Exit;
    end;
    elem := TPCElement(DSSPrime.ActiveCircuit.ActiveCktElement);
    DSSPrime.API_VarIdx := elem.LookupVariable(Value);
    if (DSSPrime.API_VarIdx <= 0) or (DSSPrime.API_VarIdx > elem.NumVariables) then
        DoSimpleMsg(DSSPrime, 'Invalid variable name "%s" for "%s"', [Value, elem.FullName], 100001);
end;
//------------------------------------------------------------------------------
function CktElement_Get_VariableValue(): Double; CDECL;
var
    elem: TPCElement;
begin
    Result := 0;
    if InvalidCktElement(DSSPrime, True) then
        Exit;

    elem := TPCElement(DSSPrime.ActiveCircuit.ActiveCktElement);
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
    elem: TPCElement;
begin
    if InvalidCktElement(DSSPrime, True) then
        Exit;

    elem := TPCElement(DSSPrime.ActiveCircuit.ActiveCktElement);
    if (DSSPrime.API_VarIdx <= 0) or (DSSPrime.API_VarIdx > elem.NumVariables) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid variable index %d for "%s"', [DSSPrime.API_VarIdx, elem.FullName], 100002);
        Exit;
    end;
    elem.Variable[DSSPrime.API_VarIdx] := Value;
end;
//------------------------------------------------------------------------------
function CktElement_Get_VariableIdx(): Integer; CDECL;
begin
    Result := -1;
    if InvalidCktElement(DSSPrime, True) then
        Exit;
    Result := DSSPrime.API_VarIdx;
end;
//------------------------------------------------------------------------------
procedure CktElement_Set_VariableIdx(Value: Integer); CDECL;
var
    elem: TPCElement;
begin
    if InvalidCktElement(DSSPrime, True) then
    begin
        DSSPrime.API_VarIdx := -1;
        Exit;
    end;

    elem := TPCElement(DSSPrime.ActiveCircuit.ActiveCktElement);
    if (Value <= 0) or (Value > elem.NumVariables) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid variable index %d for "%s"', [DSSPrime.API_VarIdx, elem.FullName], 100003);
        Exit;
    end;
    DSSPrime.API_VarIdx := Value;
end;
//------------------------------------------------------------------------------
function Obj_CktElement_MaxCurrent(obj: TDSSCktElement; terminalIdx: Integer): Double;
begin
    Result := obj.MaxCurrent[terminalIdx];
end;
//------------------------------------------------------------------------------
procedure Obj_Circuit_Set_ActiveCktElement(obj: TDSSCktElement);
begin
    obj.DSS.ActiveCircuit.ActiveCktElement := obj;
end;
//------------------------------------------------------------------------------
end.
