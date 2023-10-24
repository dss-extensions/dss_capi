unit CAPI_Alt;

interface

uses
    CAPI_Utils,
    DSSObject,
    CktElement,
    PCElement,
    PDElement,
    LoadShape,
    Monitor,
    Transformer,
    EnergyMeter,
    CAPI_Types;

procedure Alt_CE_Get_BusNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_NumConductors(elem: TDSSCktElement): Integer; CDECL;
function Alt_CE_Get_NumPhases(elem: TDSSCktElement): Integer; CDECL;
function Alt_CE_Get_NumTerminals(elem: TDSSCktElement): Integer; CDECL;
procedure Alt_CE_Set_BusNames(elem: TDSSCktElement; ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
procedure Alt_CE_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_Losses(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_PhaseLosses(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_Powers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Close(elem: TDSSCktElement; Term, Phs: Integer); CDECL;
procedure Alt_CE_Open(elem: TDSSCktElement; Term, Phs: Integer); CDECL;
function Alt_CE_IsOpen(elem: TDSSCktElement; Term, Phs: Integer): TAPIBoolean; CDECL;
procedure Alt_CE_Get_Residuals(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_Yprim(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_Handle(elem: TDSSCktElement): Integer; CDECL;
// function Alt_CE_Get_ControllerName(elem: TDSSCktElement; idx: Integer): PAnsiChar; CDECL;
// function Alt_CE_Get_Controller(elem: TDSSCktElement; idx: Integer): TDSSObject; CDECL;
procedure Alt_CE_Get_Controllers(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_HasVoltControl(elem: TDSSCktElement): TAPIBoolean; CDECL;
function Alt_CE_Get_HasSwitchControl(elem: TDSSCktElement): TAPIBoolean; CDECL;
procedure Alt_CE_Get_ComplexSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_ComplexSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_NodeOrder(var ResultPtr: PInteger; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_HasOCPDevice(elem: TDSSCktElement): TAPIBoolean; CDECL;
function Alt_CE_Get_NumControllers(elem: TDSSCktElement): Integer; CDECL;
function Alt_CE_Get_OCPDevice(elem: TDSSCktElement): TDSSCktElement; CDECL;
function Alt_CE_Get_OCPDeviceIndex(elem: TDSSCktElement): Integer; CDECL;
function Alt_CE_Get_OCPDeviceType(elem: TDSSCktElement): Integer; CDECL;
procedure Alt_CE_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_TotalPowers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_IsIsolated(elem: TDSSCktElement): TAPIBoolean; CDECL;
procedure Alt_CE_Get_NodeRef(var ResultPtr: PInteger; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_DisplayName(elem: TDSSCktElement): PAnsiChar; CDECL;
function Alt_CE_Get_GUID(elem: TDSSCktElement): PAnsiChar; CDECL;
procedure Alt_CE_Set_DisplayName(elem: TDSSCktElement; const Value: PAnsiChar); CDECL;
function Alt_CE_MaxCurrent(obj: TDSSCktElement; terminalIdx: Integer): Double; CDECL;
procedure Alt_CE_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
//PCElements
procedure Alt_PCE_Get_VariableNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TPCElement); CDECL;
procedure Alt_PCE_Get_VariableValues(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TPCElement); CDECL;
procedure Alt_PCE_Set_VariableValue(elem: TPCElement; varIdx: Integer; Value: Double); CDECL;
function Alt_PCE_Get_VariableValue(elem: TPCElement; varIdx: Integer): Double; CDECL;
function Alt_PCE_Get_VariableName(elem: TPCElement; varIdx: Integer): PAnsiChar; CDECL;
function Alt_PCE_Get_EnergyMeter(elem: TPCElement): TDSSObject; CDECL;
function Alt_PCE_Get_EnergyMeterName(elem: TPCElement): PAnsiChar; CDECL;

//PDElements
function Alt_PDE_Get_EnergyMeter(elem: TPDElement): TDSSObject; CDECL;
function Alt_PDE_Get_EnergyMeterName(elem: TPDElement): PAnsiChar; CDECL;
function Alt_PDE_Get_IsShunt(elem: TPDElement): TAPIBoolean; CDECL;
function Alt_PDE_Get_AccumulatedL(elem: TPDElement): Double; CDECL;
function Alt_PDE_Get_Lambda(elem: TPDElement): Double; CDECL;
function Alt_PDE_Get_NumCustomers(elem: TPDElement): Integer; CDECL;
function Alt_PDE_Get_ParentPDElement(elem: TPDElement): TPDElement; CDECL;
function Alt_PDE_Get_TotalCustomers(elem: TPDElement): Integer; CDECL;
function Alt_PDE_Get_FromTerminal(elem: TPDElement): Integer; CDECL;
function Alt_PDE_Get_TotalMiles(elem: TPDElement): Double; CDECL;
function Alt_PDE_Get_SectionID(elem: TPDElement): Integer; CDECL;
//LoadShape
procedure Alt_LoadShape_Set_Points(elem: TLoadshapeObj; Npts: TAPISize; HoursPtr: Pointer; PMultPtr: Pointer; QMultPtr: Pointer; ExternalMemory: TAPIBoolean; IsFloat32: TAPIBoolean; Stride: Integer); CDECL;
procedure Alt_LoadShape_UseFloat64(elem: TLoadshapeObj); CDECL;
procedure Alt_LoadShape_UseFloat32(elem: TLoadshapeObj); CDECL;
//Monitor
procedure Alt_Monitor_Get_ByteStream(var ResultPtr: PByte; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
function Alt_Monitor_Get_SampleCount(pmon: TMonitorObj): Integer; CDECL;
function Alt_Monitor_Get_FileName(pmon: TMonitorObj): PAnsiChar; CDECL;
function Alt_Monitor_Get_NumChannels(pmon: TMonitorObj): Integer; CDECL;
function Alt_Monitor_Get_RecordSize(pmon: TMonitorObj): Integer; CDECL;
procedure Alt_Monitor_Show(pmon: TMonitorObj); CDECL;
procedure Alt_Monitor_Get_Channel(var ResultPtr: PDouble; ResultCount: PAPISize; pmon: TMonitorObj; Index: Integer); CDECL;
procedure Alt_Monitor_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
procedure Alt_Monitor_Get_dblHour(var ResultPtr: PDouble; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
procedure Alt_Monitor_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
//Transformer
procedure Alt_Transformer_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TTransfObj); CDECL;
procedure Alt_Transformer_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TTransfObj); CDECL;
procedure Alt_Transformer_Get_LossesByType(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TTransfObj); CDECL;
//EnergyMeter, general
function Alt_Meter_Get_TotalCustomers(elem: TEnergyMeterObj): Integer; CDECL;
procedure Alt_Meter_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Set_CalcCurrent(elem: TEnergyMeterObj; ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Alt_Meter_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Set_AllocFactors(elem: TEnergyMeterObj; ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Alt_Meter_DoReliabilityCalc(elem: TEnergyMeterObj; AssumeRestoration: TAPIBoolean); CDECL;
function Alt_Meter_Get_NumEndElements(elem: TEnergyMeterObj): Integer; CDECL;
function Alt_Meter_Get_NumSections(elem: TEnergyMeterObj): Integer; CDECL;
function Alt_Meter_Get_NumBranchesInZone(elem: TEnergyMeterObj): Integer; CDECL;
procedure Alt_Meter_Get_ZonePCEs(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Get_EndElements(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Get_BranchesInZone(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Get_SequenceList(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Get_Loads(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
//MeterSection
function Alt_MeterSection_AvgRepairTime(elem: TEnergyMeterObj; idx: Integer): Double; CDECL;
function Alt_MeterSection_FaultRateXRepairHours(elem: TEnergyMeterObj; idx: Integer): Double; CDECL;
function Alt_MeterSection_NumBranches(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
function Alt_MeterSection_NumCustomers(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
function Alt_MeterSection_OCPDeviceType(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
function Alt_MeterSection_SumBranchFaultRates(elem: TEnergyMeterObj; idx: Integer): Double; CDECL;
function Alt_MeterSection_SequenceIndex(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
function Alt_MeterSection_TotalCustomers(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;


implementation

uses
    CAPI_Constants,
    DSSObjectHelper,
    DSSClassDefs,
    DSSGlobals,
    UComplex, DSSUcomplex,
    Sysutils,
    MathUtil,
    Utilities,
    DSSClass,
    DSSHelper,
    Bus,
    Solution,
    PDClass,
    DSSPointerList,
    XYCurve,
    ArrayDef,
    Math,
    Classes,
    CktElementClass,
    CktTree;
    
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

        Exit;
    end;
    
    // for 3-phase elements
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
    NodeV := elem.DSS.ActiveCircuit.Solution.NodeV;

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

        Exit;
    end;
    
    // for 3-phase elements
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

//------------------------------------------------------------------------------
function IsPDElement(elem: TDSSCktElement): Boolean;
begin
    Result := ((elem.DSSObjType and 3) = PD_ELEMENT)
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_BusNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    Result: PPAnsiCharArray0;
    i: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, elem.Nterms);
    for i := 1 to elem.Nterms do
        Result[i - 1] := DSS_CopyStringAsPChar(elem.GetBus(i));

end;
//------------------------------------------------------------------------------
function Alt_CE_Get_NumConductors(elem: TDSSCktElement): Integer; CDECL;
begin
    Result := elem.NConds
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_NumTerminals(elem: TDSSCktElement): Integer; CDECL;
begin
    Result := elem.NTerms
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Set_BusNames(elem: TDSSCktElement; ValuePtr: PPAnsiChar; ValueCount: TAPISize); CDECL;
var
    Value: PPAnsiCharArray0;
    i: Integer;
    Count: Integer;
begin
    Value := PPAnsiCharArray0(ValuePtr);
    Count := ValueCount;
    if (Count <> elem.NTerms) AND (DSS_CAPI_EXT_ERRORS) then
    begin
        DoSimpleMsg(elem.DSS, 'The number of buses provided (%d) does not match the number of terminals (%d).', [ValueCount, Integer(elem.NTerms)], 97895);
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
procedure Alt_CE_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    Result: PDoubleArray0;
begin
    DefaultResult(ResultPtr, ResultCount);
    if MissingSolution(elem) then
        Exit;
        
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * (elem.NConds * elem.NTerms), elem.NConds, elem.NTerms);
    elem.GetCurrents(pComplexArray(Result));
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// Bus Voltages at active terminal
var
    Result: PDoubleArray0;
    i, n, iV: Integer;
    Volts: Complex;

    NodeV: pNodeVArray;
begin
    // Return voltages for all terminals
    DefaultResult(ResultPtr, ResultCount);
    
    if MissingSolution(elem) or (elem.NodeRef = NIL) then
        Exit;

    NodeV := elem.DSS.ActiveCircuit.Solution.NodeV;
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
//------------------------------------------------------------------------------
procedure Alt_CE_Get_Losses(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    Result: PDoubleArray0;
    LossValue: complex;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if MissingSolution(elem) then
        Exit;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    LossValue := elem.Losses;
    Result[0] := LossValue.re;
    Result[1] := LossValue.im;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_PhaseLosses(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// Returns Phase losses in kW, kVar
var
    Result: PDoubleArray0;
    NValues, i: Integer;
begin
    if MissingSolution(elem) then
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
//------------------------------------------------------------------------------
procedure Alt_CE_Get_Powers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// Return complex kW, kvar in each conductor for each terminal
var
    Result: PDoubleArray0;
    NValues,
    i: Integer;
begin
    if MissingSolution(elem) then
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
//------------------------------------------------------------------------------
procedure Alt_CE_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// All sequence currents of active ciruit element
// returns magnitude only.
var
    Result: PDoubleArray0;
    i: Integer;
    i012: pComplexArray;
    S: String;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if MissingSolution(elem) or (not elem.Enabled) then
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
            DoSimpleMsg(elem.DSS, S, 5012);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// All seq Powers of active 3-phase circuit element
// returns kW + j kvar
var
    Result: PDoubleArray0;
    Nvalues, i, j, k, n, icount: Integer;
    S: Complex;
    VPh, V012: Complex3;
    IPh, I012: Complex3;
    cBuffer: pComplexArray;
    NodeV: pNodeVArray;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if MissingSolution(elem) or (elem.NodeRef = NIL) then // or (not elem.Enabled)
        Exit;

    NodeV := elem.DSS.ActiveCircuit.Solution.NodeV;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * elem.NTerms, 3, elem.NTerms); // allocate for kW and kvar
    if elem.NPhases <> 3 then
    begin
        if (elem.Nphases = 1) and elem.DSS.ActiveCircuit.PositiveSequence then
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
//------------------------------------------------------------------------------
procedure Alt_CE_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
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
    
    if MissingSolution(elem) or (not elem.Enabled) or (elem.NodeRef = NIL) then
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
            DoSimpleMsg(elem.DSS, S, 5012);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Close(elem: TDSSCktElement; Term, Phs: Integer); CDECL;
begin
    //TODO: why is this changing ActiveTerminal directly?
    elem.ActiveTerminal := @elem.Terminals[Term - 1];
    elem.Closed[Phs] := TRUE;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Open(elem: TDSSCktElement; Term, Phs: Integer); CDECL;
begin
    //TODO: why is this changing ActiveTerminal directly?
    elem.ActiveTerminal := @elem.Terminals[Term - 1];
    elem.Closed[Phs] := FALSE;
end;
//------------------------------------------------------------------------------
function Alt_CE_IsOpen(elem: TDSSCktElement; Term, Phs: Integer): TAPIBoolean; CDECL;
var
    i: Integer;
begin
    Result := False;
    
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
procedure Alt_CE_Get_Residuals(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    Result: PDoubleArray0;
    cBuffer: pComplexArray;
    iV, i, j, k: Integer;
    cResid: Complex;
begin
    if MissingSolution(elem) then
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
//------------------------------------------------------------------------------
procedure Alt_CE_Get_Yprim(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    cValues: pComplexArray;
begin
    DefaultResult(ResultPtr, ResultCount);
    cValues := elem.GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
    if cValues = NIL then 
        Exit;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * elem.Yorder * elem.Yorder, elem.Yorder, elem.Yorder);
    Move(cValues^, ResultPtr^, ResultCount^ * SizeOf(Double));
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_Handle(elem: TDSSCktElement): Integer; CDECL;
begin
    Result := elem.Handle
end;
//------------------------------------------------------------------------------
// function Alt_CE_Get_ControllerName(elem: TDSSCktElement; idx: Integer): PAnsiChar; CDECL;
// var
//     ctrl: TDSSCktElement;
// begin
//     Result := NIL;
//     if (idx > 0) and (idx <= elem.ControlElementList.Count) then
//     begin
//         ctrl := elem.ControlElementList.Get(idx);
//         if ctrl <> NIL then
//             Result := DSS_GetAsPAnsiChar(elem.DSS, ctrl.FullName);
//     end;
// end;
// //------------------------------------------------------------------------------
// function Alt_CE_Get_Controller(elem: TDSSCktElement; idx: Integer): TDSSObject; CDECL;
// begin
//     Result := NIL;
//     if (idx > 0) and (idx <= elem.ControlElementList.Count) then
//     begin
//         Result := elem.ControlElementList.Get(idx);
//     end;
// end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_Controllers(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
begin
    ResultCount[0] := 0;
    if elem.ControlElementList = NIL then
        Exit;

    DSS_RecreateArray_PPointer(ResultPtr, ResultCount, elem.ControlElementList.Count);
    Move(elem.ControlElementList.InternalPointer^, ResultPtr^, ResultCount^ * SizeOf(Pointer));
end;
//------------------------------------------------------------------------------
function Alt_PCE_Get_EnergyMeterName(elem: TPCElement): PAnsiChar; CDECL;
begin
    Result := NIL;
    if Flg.HasEnergyMeter in elem.Flags then
    begin
        Result := PAnsiChar(elem.MeterObj.Name);
    end;
end;

function Alt_PCE_Get_EnergyMeter(elem: TPCElement): TDSSObject; CDECL;
begin
    Result := NIL;
    if Flg.HasEnergyMeter in elem.Flags then
    begin
        Result := elem.MeterObj;
    end;
end;

function Alt_PDE_Get_EnergyMeterName(elem: TPDElement): PAnsiChar; CDECL;
begin
    Result := NIL;
    if Flg.HasEnergyMeter in elem.Flags then
    begin
        Result := PAnsiChar(elem.MeterObj.Name);
    end;
end;

function Alt_PDE_Get_EnergyMeter(elem: TPDElement): TDSSObject; CDECL;
begin
    Result := NIL;
    if Flg.HasEnergyMeter in elem.Flags then
    begin
        Result := elem.MeterObj;
    end;
end;

//------------------------------------------------------------------------------
function Alt_CE_Get_HasVoltControl(elem: TDSSCktElement): TAPIBoolean; CDECL;
// Returns true if any of the controls is a capcontrol or a regcontrol
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;
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
function Alt_CE_Get_HasSwitchControl(elem: TDSSCktElement): TAPIBoolean; CDECL;
var
    ctrl: TDSSCktElement;
begin
    Result := FALSE;
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
procedure Alt_CE_Get_ComplexSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    S: String;
begin
    DefaultResult(ResultPtr, ResultCount);

    if MissingSolution(elem) or (not elem.Enabled) or (elem.NodeRef = NIL) then
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
            DoSimpleMsg(elem.DSS, S, 5012);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_ComplexSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    Result: PDoubleArray0;
    i012: pComplexArray;
    S: String;
begin
    DefaultResult(ResultPtr, ResultCount);
    
    if MissingSolution(elem) or (not elem.Enabled) then
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
            DoSimpleMsg(elem.DSS, S, 5012);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_PCE_Get_VariableNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TPCElement); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, elem.NumVariables);
    for k := 1 to elem.NumVariables do
    begin
        Result[k - 1] := DSS_CopyStringAsPChar(elem.VariableName(k));
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_PCE_Get_VariableValues(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TPCElement); CDECL;
// Return array of doubles with values of all variables if PCElement
var
    Result: PDoubleArray0;
    k: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NumVariables);
    for k := 1 to elem.NumVariables do
    begin
        Result[k - 1] := elem.Variable[k];
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_NodeOrder(var ResultPtr: PInteger; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    Result: PIntegerArray0;
    k: Integer;
    i: Integer;
    j: Integer;
begin
    if elem.NodeRef = NIL then
    begin
        // Warn and exit
        DoSimpleMsg(elem.DSS, _('Nodes are not initialized. Try solving the system first.'), 15013);
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, elem.NTerms * elem.Nconds, elem.NTerms, elem.Nconds);
    k := 0;
    for i := 1 to elem.Nterms do
    begin
        for j := (i - 1) * elem.NConds + 1 to i * elem.Nconds do
        begin
            Result[k] := GetNodeNum(elem.DSS, elem.NodeRef[j]);
            inc(k);
        end;
    end;
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_HasOCPDevice(elem: TDSSCktElement): TAPIBoolean; CDECL;
// Check for presence of a fuse, recloser, etc.
begin
    Result := Flg.HasOCPDevice in elem.Flags;
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_NumControllers(elem: TDSSCktElement): Integer; CDECL;
begin
    Result := elem.ControlElementList.Count;
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_OCPDevice(elem: TDSSCktElement): TDSSCktElement; CDECL;
var
    iControl: Integer;
    ctrl: TDSSCktElement;
begin
    Result := NIL;
    iControl := 1;
    repeat
        // cycle through the list of controls until we find a fuse, recloser, or relay
        ctrl := elem.ControlElementList.Get(iControl);
        if ctrl <> NIL then
            case (ctrl.DSSObjType and CLASSMASK) of
                FUSE_CONTROL:
                    Result := ctrl;
                RECLOSER_CONTROL:
                    Result := ctrl;
                RELAY_CONTROL:
                    Result := ctrl;
            end;
        inc(iControl);
    until (iControl > elem.ControlElementList.Count) or (Result <> NIL);
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_OCPDeviceIndex(elem: TDSSCktElement): Integer; CDECL;
var
    iControl: Integer;
    ctrl: TDSSCktElement;
begin
    Result := 0;

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
function Alt_CE_Get_OCPDeviceType(elem: TDSSCktElement): Integer; CDECL;
begin
    Result := GetOCPDeviceType(elem);     // see Utilities.pas
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// return currents in magnitude, angle array
var
    Result: PDoubleArray0;
    cBuffer: pComplexArray;
    CMagAng: polar;
    NValues, iV, i: Integer;
begin
    if MissingSolution(elem) then
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
//------------------------------------------------------------------------------
procedure Alt_CE_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// Bus Voltages in magnitude, angle at all terminal
var
    Result: PDoubleArray0;
    numcond, i, n, iV: Integer;
    Volts: Polar;
    NodeV: pNodeVArray;
begin
    // Return voltages for all terminals
    if MissingSolution(elem) or (elem.NodeRef = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    NodeV := elem.DSS.ActiveCircuit.Solution.NodeV;

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
//------------------------------------------------------------------------------
function Alt_CE_Get_IsIsolated(elem: TDSSCktElement): TAPIBoolean; CDECL;
begin
    Result := Flg.IsIsolated in elem.Flags;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_TotalPowers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
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
    if MissingSolution(elem) or (elem.NodeRef = NIL) then
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
procedure Alt_CE_Get_NodeRef(var ResultPtr: PInteger; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;    
begin
    DefaultResult(ResultPtr, ResultCount);
    if elem.NodeRef = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(elem.DSS, _('NodeRef is not populated for the current element!'), 97801);
        end;
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, elem.Yorder);
    Move(elem.NodeRef[1], ResultPtr^, elem.Yorder * SizeOf(Integer));
end;
//------------------------------------------------------------------------------
function Alt_CE_MaxCurrent(obj: TDSSCktElement; terminalIdx: Integer): Double; CDECL;
begin
    Result := obj.MaxCurrent[terminalIdx];
end;
//------------------------------------------------------------------------------
function Alt_PCE_Get_VariableName(elem: TPCElement; varIdx: Integer): PAnsiChar; CDECL;
begin
    Result := NIL;
    if (varIdx <= 0) or (varIdx > elem.NumVariables) then
    begin
        DoSimpleMsg(elem.DSS, 'Invalid variable index %d for "%s"', [varIdx, elem.FullName], 97802);
        Exit;
    end;
    Result := DSS_GetAsPAnsiChar(elem.DSS, elem.VariableName(varIdx));
end;
//------------------------------------------------------------------------------
function Alt_PCE_Get_VariableValue(elem: TPCElement; varIdx: Integer): Double; CDECL;
begin
    Result := 0;
    if (varIdx <= 0) or (varIdx > elem.NumVariables) then
    begin
        DoSimpleMsg(elem.DSS, 'Invalid variable index %d for "%s"', [varIdx, elem.FullName], 100002);
        Exit;
    end;
    Result := elem.Variable[varIdx];
end;
//------------------------------------------------------------------------------
procedure Alt_PCE_Set_VariableValue(elem: TPCElement; varIdx: Integer; Value: Double); CDECL;
begin
    if (varIdx <= 0) or (varIdx > elem.NumVariables) then
    begin
        if DSS_CAPI_EXT_ERRORS then
            DoSimpleMsg(elem.DSS, 'Invalid variable index %d for "%s"', [varIdx, elem.FullName], 100002);
        Exit;
    end;
    elem.Variable[varIdx] := Value;
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_NumPhases(elem: TDSSCktElement): Integer; CDECL;
begin
    Result := elem.NPhases
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_Name(elem: TDSSCktElement): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(elem.DSS, elem.FullName);
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_DisplayName(elem: TDSSCktElement): PAnsiChar; CDECL;
begin
    if elem.DisplayName <> '' then
        Result := DSS_GetAsPAnsiChar(elem.DSS, elem.DisplayName)
    else
        Result := DSS_GetAsPAnsiChar(elem.DSS, elem.ParentClass.Name + '_' + elem.Name);
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_GUID(elem: TDSSCktElement): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(elem.DSS, elem.ID)
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Set_DisplayName(elem: TDSSCktElement; const Value: PAnsiChar); CDECL;
begin
    elem.DisplayName := Value;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_IsShunt(elem: TPDElement): TAPIBoolean; CDECL;
begin
    Result := elem.IsShunt;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_AccumulatedL(elem: TPDElement): Double; CDECL;
begin
    Result := elem.AccumulatedBrFltRate;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_Lambda(elem: TPDElement): Double; CDECL;
begin
    Result := elem.BranchFltRate;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_NumCustomers(elem: TPDElement): Integer; CDECL;
begin
    Result := elem.BranchNumCustomers;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_ParentPDElement(elem: TPDElement): TPDElement; CDECL;
begin
    Result := elem.ParentPDElement;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_TotalCustomers(elem: TPDElement): Integer; CDECL;
begin
    Result := elem.BranchTotalCustomers;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_FromTerminal(elem: TPDElement): Integer; CDECL;
begin
    Result := elem.FromTerminal;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_TotalMiles(elem: TPDElement): Double; CDECL;
// Total miles of line from here on down to the end of the feeder
begin
    Result := elem.AccumulatedMilesDownStream;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_SectionID(elem: TPDElement): Integer; CDECL;
begin
    Result := elem.BranchSectionID;
end;
//------------------------------------------------------------------------------
procedure Alt_LoadShape_Set_Points(elem: TLoadshapeObj; Npts: TAPISize; HoursPtr: Pointer; PMultPtr: Pointer; QMultPtr: Pointer; ExternalMemory: TAPIBoolean; IsFloat32: TAPIBoolean; Stride: Integer); CDECL;
begin
    // If the LoadShape owns the memory, dispose the current data and reallocate if necessary
    if not elem.ExternalMemory then
    begin
        ReallocMem(elem.dP, 0);
        ReallocMem(elem.dQ, 0);
        ReallocMem(elem.dH, 0);
        ReallocMem(elem.sP, 0);
        ReallocMem(elem.sQ, 0);
        ReallocMem(elem.sH, 0);
    end;
    elem.dP := NIL;
    elem.dQ := NIL;
    elem.dH := NIL;
    elem.sP := NIL;
    elem.sQ := NIL;
    elem.sH := NIL;
    
    elem.ExternalMemory := ExternalMemory;
    elem.NumPoints := Npts;

    if not ExternalMemory then
    begin
        elem.Stride := 1;
        if not IsFloat32 then
        begin
            if PMultPtr <> NIL then
            begin
                ReallocMem(elem.dP, Sizeof(Double) * Npts);
                Move(PMultPtr^, elem.dP[0], Npts * SizeOf(Double));
            end;
            if QMultPtr <> NIL then
            begin
                ReallocMem(elem.dQ, Sizeof(Double) * Npts);
                Move(QMultPtr^, elem.dQ[0], Npts * SizeOf(Double));
            end;
            if HoursPtr <> NIL then
            begin
                ReallocMem(elem.dH, Sizeof(Double) * Npts);
                Move(HoursPtr^, elem.dH[0], Npts * SizeOf(Double));
            end;
            if Assigned(elem.dP) then
                elem.SetMaxPandQ;
        end
        else // if IsFloat32
        begin
            if PMultPtr <> NIL then
            begin
                ReallocMem(elem.sP, Sizeof(Single) * Npts);
                Move(PMultPtr^, elem.sP[0], Npts * SizeOf(Single));
            end;
            if QMultPtr <> NIL then
            begin
                ReallocMem(elem.sQ, Sizeof(Single) * Npts);
                Move(QMultPtr^, elem.sQ[0], Npts * SizeOf(Single));
            end;
            if HoursPtr <> NIL then
            begin
                ReallocMem(elem.sH, Sizeof(Single) * Npts);
                Move(HoursPtr^, elem.sH[0], Npts * SizeOf(Single));
            end;
            if Assigned(elem.sP) then
                elem.SetMaxPandQ;
        end;
        Exit;
    end;
    
    // Using externally controlled memory
    if not IsFloat32 then
        elem.SetDataPointers(HoursPtr, PMultPtr, QMultPtr, Stride)
    else
        elem.SetDataPointersSingle(HoursPtr, PMultPtr, QMultPtr, Stride)
end;
//------------------------------------------------------------------------------
procedure Alt_LoadShape_UseFloat64(elem: TLoadshapeObj); CDECL;
begin
    elem.UseFloat64();
end;
//------------------------------------------------------------------------------
procedure Alt_LoadShape_UseFloat32(elem: TLoadshapeObj); CDECL;
begin
    elem.UseFloat32();
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
    names: ArrayOfString = NIL;
    cls: TDSSClass;
begin
    cls := elem.ParentClass;
    if not (cls is TCktElementClass) then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 0);
        Exit;
    end;

    names := TCktElementClass(cls).GetRegisterNames(elem);
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(names));
    for k := 0 to High(names) do
    begin
        Result[k] := DSS_CopyStringAsPChar(names[k]);
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_CE_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
var
    num: Integer = 0;
    values: PDoubleArray;
    cls: TDSSClass;
begin
    cls := elem.ParentClass;
    if not (cls is TCktElementClass) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 0);
        Exit;
    end;
    values := TCktElementClass(cls).GetRegisterValues(elem, num);
    if (num = 0) or (values = NIL) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 0);
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, num);
    Move(values^, ResultPtr^, num * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Alt_Monitor_Show(pmon: TMonitorObj); CDECL;
begin
    PMon.TranslateToCSV(TRUE);
end;
//------------------------------------------------------------------------------
procedure Alt_Monitor_Get_ByteStream(var ResultPtr: PByte; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
begin
    DSS_RecreateArray_PByte(ResultPtr, ResultCount, pmon.MonitorStream.Size);
    pmon.MonitorStream.Seek(0, soFromBeginning);
    pmon.MonitorStream.Read(ResultPtr^, pmon.MonitorStream.Size);   // Move it all over
    // leaves stream at the end
end;
//------------------------------------------------------------------------------
function Alt_Monitor_Get_SampleCount(pmon: TMonitorObj): Integer; CDECL;
begin
    Result := pmon.SampleCount;
end;
//------------------------------------------------------------------------------
function Alt_Monitor_Get_FileName(pmon: TMonitorObj): PAnsiChar; CDECL;
begin
    Result := PAnsiChar(pmon.CSVFileName);
end;
//------------------------------------------------------------------------------
function Alt_Monitor_Get_NumChannels(pmon: TMonitorObj): Integer; CDECL;
begin
    Result := pmon.RecordSize;
end;
//------------------------------------------------------------------------------
function Alt_Monitor_Get_RecordSize(pmon: TMonitorObj): Integer; CDECL;
begin
    Result := pmon.RecordSize;
end;
//------------------------------------------------------------------------------
procedure Alt_Monitor_Get_Channel(var ResultPtr: PDouble; ResultCount: PAPISize; pmon: TMonitorObj; Index: Integer); CDECL;
// Return an array of doubles for selected channel
var
    Result: PDoubleArray0;
    i: Integer;
    SngBuffer: pSingleArray;
    AllocSize: Integer;
begin
    DefaultResult(ResultPtr, ResultCount);
    if pmon.SampleCount <= 0 then
        Exit;

    pmon.MonitorStream.Seek(256 + 4 * 4, soFromBeginning); // Skip header

    if (Index < 1) or (Index > pmon.RecordSize) then // NumChannels
    begin
        DoSimpleMsg(pmon.DSS,
            'Monitors.Channel: invalid channel index (%d), monitor "%s" has %d channels.',
            [Index, pmon.Name, pmon.RecordSize],
            5888);
        Exit;
    end;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pmon.SampleCount);

    AllocSize := Sizeof(Single) * (pmon.RecordSize + 2); // Include Hour and Second fields
    Index := Index + 2; // Skip Hour and Second fields
    SngBuffer := Allocmem(AllocSize); // Need a buffer to convert from float32 to float64
    for i := 1 to pmon.SampleCount do
    begin
        pmon.MonitorStream.Read(sngBuffer[1], AllocSize);  // read rest of record
        Result[i - 1] := sngBuffer[Index];
    end;
    Reallocmem(SngBuffer, 0);  // Dispose of buffer
end;
//------------------------------------------------------------------------------
procedure Alt_Monitor_Get_dblHourFreq(pmon: TMonitorObj; freq: Boolean; var ResultPtr: PDouble; ResultCount: PAPISize); // local function
// Return an array of doubles for time in hours
var
    Result: PDoubleArray0;
    k, i: Integer;
    FirstCol: String;
    SngBuffer: pSingleArray;
    hr_freq: Single;
    sec_harm: Single;
    AllocSize: Integer;
    MonitorStream: TMemoryStream;
    FirstColTarget: String;
    smult: Double;
begin
    DefaultResult(ResultPtr, ResultCount);
    if pmon.SampleCount <= 0 then
        Exit;

    if freq then
    begin
        FirstColTarget := 'freq';
        smult := 0;
    end
    else
    begin
        FirstColTarget := 'hour';
        smult := 1.0 / 3600.0;
    end;

    MonitorStream := pmon.MonitorStream;
    MonitorStream.Seek(256 + 4 * 4, soFromBeginning); // Skip header
    FirstCol := pmon.Header.Strings[0];

    if Sysutils.CompareText(FirstCol, FirstColTarget) <> 0 then
    begin   // Not the target solution, so return nil array
        MonitorStream.Seek(0, soFromEnd); // leave stream at end
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, pmon.SampleCount);
    AllocSize := Sizeof(Single) * pmon.RecordSize;
    SngBuffer := Allocmem(AllocSize);
    k := 0;
    for i := 1 to pmon.SampleCount do
    begin
        MonitorStream.Read(hr_freq, SizeOf(hr_freq));  // Hour, or frequency
        MonitorStream.Read(sec_harm, SizeOf(sec_harm));   // Seconds past the hour, or harmonic
        MonitorStream.Read(sngBuffer[1], AllocSize);  // read rest of record
        Result[k] := hr_freq + sec_harm * smult;
        k += 1;
    end;
    Reallocmem(SngBuffer, 0);  // Dispose of buffer
end;

procedure Alt_Monitor_Get_dblFreq(var ResultPtr: PDouble; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
// Return an array of doubles for frequence for Harmonic solutions
begin
    Alt_Monitor_Get_dblHourFreq(pmon, false, ResultPtr, ResultCount);
end;

procedure Alt_Monitor_Get_dblHour(var ResultPtr: PDouble; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
// Return an array of doubles for time in hours
begin
    Alt_Monitor_Get_dblHourFreq(pmon, false, ResultPtr, ResultCount);
end;
//------------------------------------------------------------------------------
procedure Alt_Monitor_Get_Header(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; pmon: TMonitorObj); CDECL;
// Variant list of strings with names of all channels
var
    Result: PPAnsiCharArray0;
    k: Integer;
    ListSize: Integer;
begin
    DefaultResult(ResultPtr, ResultCount);
    if pmon.RecordSize <= 0 then
        Exit;

    ListSize := pmon.RecordSize;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, ListSize);

    k := 0;
    while k < ListSize do
    begin
        Result[k] := DSS_CopyStringAsPChar(pmon.Header.Strings[k + 2]);
        Inc(k);
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_Transformer_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TTransfObj); CDECL;
begin
    if (elem.ActiveWinding > 0) and (elem.ActiveWinding <= elem.NumWindings) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * elem.nphases);
        if elem.Enabled then
            elem.GetWindingVoltages(elem.ActiveWinding, pComplexArray(ResultPtr));
        Exit;
    end;
    DefaultResult(ResultPtr, ResultCount);
end;
//------------------------------------------------------------------------------
procedure Alt_Transformer_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TTransfObj); CDECL;
var
    NumCurrents: Integer;
begin
    NumCurrents := 2 * elem.NPhases * elem.NumWindings; // 2 currents per winding
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NumCurrents);
    if elem.Enabled then
        elem.GetAllWindingCurrents(pComplexArray(ResultPtr));
end;
//------------------------------------------------------------------------------
procedure Alt_Transformer_Get_LossesByType(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TTransfObj); CDECL;
// Returns an array with (TotalLosses, LoadLosses, NoLoadLosses) for the current active transformer, in VA
var 
    CResult: PComplexArray; // this array is one-based, see DSSUcomplex
begin
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3);
    CResult := PComplexArray(ResultPtr);
    elem.GetLosses(CResult[1], CResult[2], CResult[3]);
    // Keep the results in VA (NOT kVA) for consistency with CktElement_Get_Losses
end;
//------------------------------------------------------------------------------
function checkLoadList(elem: TEnergyMeterObj): Boolean;
begin
    Result := true;
    if (elem.LoadList = NIL) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            elem.DoSimpleMsg('LoadList for %s is not initialized. Try solving or running "Makebuslist" first.', [elem.FullName], 8987);
        end;
        Result := false;
    end;
end;

function checkSequenceList(elem: TEnergyMeterObj): Boolean;
begin
    Result := true;
    if (elem.SequenceList = NIL) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            elem.DoSimpleMsg('SequenceList for %s is not initialized. Try solving or running "Makebuslist" first.', [elem.FullName], 8988);
        end;
        Result := false;
    end;
end;

function Alt_Meter_Get_TotalCustomers(elem: TEnergyMeterObj): Integer; CDECL;
var
    PD_Element: TPDElement;
begin
    Result := 0;
    if (not checkSequenceList(elem)) or (elem.Circuit.Buses = NIL) then 
        Exit;

    PD_Element := elem.SequenceList.Get(1);
    if PD_Element = NIL then
        Exit;
        
    Result := elem.ActiveCircuit.Buses[PD_Element.Terminals[PD_Element.FromTerminal - 1].BusRef].BusTotalNumCustomers;
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
var
    Result: PDoubleArray0;
    k: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NPhases);
    for k := 0 to elem.NPhases - 1 do
        Result[k] := Cabs(elem.CalculatedCurrent[k + 1]);
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Set_CalcCurrent(elem: TEnergyMeterObj; ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    i: Integer;
begin
    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5025);
        Exit;
    end;
    Value := PDoubleArray0(ValuePtr);
    for i := 1 to elem.NPhases do
        elem.CalculatedCurrent[i] := Value[i - 1];   // Just set the real part
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
begin
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.NPhases);
    Move(elem.PhsAllocationFactor[1], ResultPtr^, ResultCount^ * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Set_AllocFactors(elem: TEnergyMeterObj; ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    i: Integer;
begin
    Value := PDoubleArray0(ValuePtr);
    if ValueCount <> elem.NPhases then
    begin
        DoSimpleMsg(DSSPrime, _('The provided number of values does not match the element''s number of phases.'), 5026);
        Exit;
    end;
    for i := 1 to elem.NPhases do
    begin
        elem.PhsAllocationFactor[i] := Value[i - 1];
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_DoReliabilityCalc(elem: TEnergyMeterObj; AssumeRestoration: TAPIBoolean); CDECL;
begin
    elem.AssumeRestoration := AssumeRestoration;
    elem.CalcReliabilityIndices();
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_ZonePCEs(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
var
   k: integer;
begin
    elem.GetPCEatZone(True);
    if not ((Length(elem.ZonePCE) > 0) and (elem.ZonePCE[0] <> NIL)) then
        Exit;
        
    DSS_RecreateArray_PPointer(ResultPtr, ResultCount, length(elem.ZonePCE));
    Move(elem.ZonePCE[0], ResultPtr^, ResultCount^ * SizeOf(Pointer));
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_EndElements(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
var
    Result: PPointerArray0;
    k, num: Integer;
    node: TCktTreeNode;
begin
    ResultCount[0] := 0;
    if not elem.CheckBranchList(5502) then
        Exit;
    if elem.BranchList.ZoneEndsList = NIL then
        Exit;

    num := elem.BranchList.ZoneEndsList.NumEnds;
    DSS_RecreateArray_PPointer(Result, ResultPtr, ResultCount, num);
    for k := 0 to num - 1 do
    begin
        elem.BranchList.ZoneEndsList.Get(k + 1, node);
        Result[k] := node.CktObject;
    end;
end;
//------------------------------------------------------------------------------
function Alt_Meter_Get_NumEndElements(elem: TEnergyMeterObj): Integer; CDECL;
begin
    Result := 0;
    if not elem.CheckBranchList(5500) then
        Exit;

    if elem.BranchList.ZoneEndsList = NIL then
        Exit;

    Result := elem.BranchList.ZoneEndsList.NumEnds;
end;
//------------------------------------------------------------------------------
function Alt_Meter_Get_NumSections(elem: TEnergyMeterObj): Integer; CDECL;
begin
    Result := elem.SectionCount;
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_BranchesInZone(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
var
    Result: PPointerArray0;
    k: Integer;
    BranchCount: Integer;
    pElem: TDSSCktElement;
begin
    ResultCount[0] := 0;
    if not elem.CheckBranchList(5501) then
        Exit;

    // Get count of branches
    BranchCount := Alt_Meter_Get_NumBranchesInZone(elem);
    if BranchCount <= 0 then 
        Exit;
        
    DSS_RecreateArray_PPointer(Result, ResultPtr, ResultCount, BranchCount);
    pElem := elem.BranchList.First();
    k := 0;
    while pElem <> NIL do
    begin
        Result[k] := pElem;
        inc(k);
        pElem := elem.BranchList.GoForward();
    end;
end;
//------------------------------------------------------------------------------
function Alt_Meter_Get_NumBranchesInZone(elem: TEnergyMeterObj): Integer; CDECL;
var
    pElem : TDSSCktElement;
begin
    Result := 0;
    if elem.BranchList = NIL then //TODO: error/warning?
        Exit;

    pElem := elem.BranchList.First();
    while pElem <> NIL do
    begin
        Inc(Result);
        pElem := elem.BranchList.GoForward();
    end;
end;
//------------------------------------------------------------------------------
function checkSectionIdx(elem: TEnergyMeterObj; idx: Integer; out psec: PFeederSection): Boolean;
begin
    if (idx <= 0) or (idx > elem.SectionCount) then
    begin
        if DSS_CAPI_EXT_ERRORS then
            elem.DoSimpleMsg(_('Invalid active section. Has SetActiveSection been called?'), 5055);
        psec := NIL;
        Result := false;
        Exit;
    end;
    psec := @elem.FeederSections[idx];
    Result := true;
end;

function Alt_MeterSection_AvgRepairTime(elem: TEnergyMeterObj; idx: Integer): Double; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.AverageRepairTime;
end;

function Alt_MeterSection_FaultRateXRepairHours(elem: TEnergyMeterObj; idx: Integer): Double; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.SumFltRatesXRepairHrs;
end;

function Alt_MeterSection_NumBranches(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.NBranches;
end;

function Alt_MeterSection_NumCustomers(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.NCustomers;
end;

function Alt_MeterSection_OCPDeviceType(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.OCPDeviceType;
end;

function Alt_MeterSection_SumBranchFaultRates(elem: TEnergyMeterObj; idx: Integer): Double; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.SumBranchFltRates;
end;

function Alt_MeterSection_SequenceIndex(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.SeqIndex;
end;

function Alt_MeterSection_TotalCustomers(elem: TEnergyMeterObj; idx: Integer): Integer; CDECL;
var
    psection: PFeederSection;
begin
    Result := 0;
    if not checkSectionIdx(elem, idx, psection) then
        Exit;
    Result := psection^.TotalCustomers;
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_SequenceList(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
begin
    ResultCount[0] := 0;
    if (not checkSequenceList(elem)) then
        Exit;

    DSS_RecreateArray_PPointer(ResultPtr, ResultCount, elem.SequenceList.Count);
    Move(elem.SequenceList.InternalPointer^, ResultPtr^, ResultCount^ * SizeOf(Pointer));
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_Loads(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
begin
    ResultCount[0] := 0;
    if (not checkLoadList(elem)) then
        Exit;

    DSS_RecreateArray_PPointer(ResultPtr, ResultCount, elem.LoadList.Count);
    Move(elem.LoadList.InternalPointer^, ResultPtr^, ResultCount^ * SizeOf(Pointer));
end;
//------------------------------------------------------------------------------
end.
