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
    Bus,
    ControlledTransformer,
    EnergyMeter,
    CAPI_Types,
    fpjson;

type
    dss_ctx_bus_float64_function_t = function (ctx: Pointer; obj: Pointer): Double; CDECL;
    dss_ctx_bus_int32_function_t = function (ctx: Pointer; obj: Pointer): Integer; CDECL;
    TDSSCktElementPtr = ^TDSSCktElement;

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
function Alt_CE_IsOpen(elem: TDSSCktElement; Term, Phs: Integer): TAltAPIBoolean; CDECL;
procedure Alt_CE_Get_Residuals(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_YPrim(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_Handle(elem: TDSSCktElement): Integer; CDECL;
// function Alt_CE_Get_ControllerName(elem: TDSSCktElement; idx: Integer): PAnsiChar; CDECL;
// function Alt_CE_Get_Controller(elem: TDSSCktElement; idx: Integer): TDSSObject; CDECL;
procedure Alt_CE_Get_Controllers(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_HasVoltControl(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
function Alt_CE_Get_HasSwitchControl(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
procedure Alt_CE_Get_ComplexSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_ComplexSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_NodeOrder(var ResultPtr: PInteger; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_HasOCPDevice(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
function Alt_CE_Get_NumControllers(elem: TDSSCktElement): Integer; CDECL;
function Alt_CE_Get_OCPDevice(elem: TDSSCktElement): TDSSCktElement; CDECL;
function Alt_CE_Get_OCPDeviceIndex(elem: TDSSCktElement): Integer; CDECL;
function Alt_CE_Get_OCPDeviceType(elem: TDSSCktElement): Integer; CDECL;
procedure Alt_CE_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_TotalPowers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_IsIsolated(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
procedure Alt_CE_Get_NodeRef(var ResultPtr: PInteger; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
function Alt_CE_Get_DisplayName(elem: TDSSCktElement): PAnsiChar; CDECL;
function Alt_CE_Get_GUID(elem: TDSSCktElement): PAnsiChar; CDECL;
procedure Alt_CE_Set_DisplayName(elem: TDSSCktElement; const value: PAnsiChar); CDECL;
function Alt_CE_MaxCurrent(obj: TDSSCktElement; terminalIdx: Integer): Double; CDECL;
procedure Alt_CE_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
procedure Alt_CE_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;

procedure Alt_CEBatch_Get_Losses(var resultPtr: PDouble; resultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: TAPISize); CDECL;
procedure Alt_CEBatch_Get_PhaseLosses(var resultPtr: PDouble; resultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: TAPISize); CDECL;
procedure Alt_CEBatch_Get_TotalPowers(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_Powers(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_ComplexSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_ComplexSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
procedure Alt_CEBatch_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;

//PCElements
procedure Alt_PCE_Get_VariableNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize; elem: TPCElement); CDECL;
procedure Alt_PCE_Get_VariableValues(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TPCElement); CDECL;
procedure Alt_PCE_Set_VariableValue(elem: TPCElement; varIdx: Integer; value: Double); CDECL;
function Alt_PCE_Get_VariableValue(elem: TPCElement; varIdx: Integer): Double; CDECL;
procedure Alt_PCE_Set_VariableSValue(elem: TPCElement; varName: PAnsiChar; value: Double); CDECL;
function Alt_PCE_Get_VariableSValue(elem: TPCElement; varName: PAnsiChar): Double; CDECL;
function Alt_PCE_Get_VariableName(elem: TPCElement; varIdx: Integer): PAnsiChar; CDECL;
function Alt_PCE_Get_EnergyMeter(elem: TPCElement): TDSSObject; CDECL;
function Alt_PCE_Get_EnergyMeterName(elem: TPCElement): PAnsiChar; CDECL;

//PDElements
function Alt_PDE_Get_EnergyMeter(elem: TPDElement): TDSSObject; CDECL;
function Alt_PDE_Get_EnergyMeterName(elem: TPDElement): PAnsiChar; CDECL;
function Alt_PDE_Get_IsShunt(elem: TPDElement): TAltAPIBoolean; CDECL;
function Alt_PDE_Get_AccumulatedL(elem: TPDElement): Double; CDECL;
function Alt_PDE_Get_Lambda(elem: TPDElement): Double; CDECL;
function Alt_PDE_Get_NumCustomers(elem: TPDElement): Integer; CDECL;
function Alt_PDE_Get_ParentPDElement(elem: TPDElement): TPDElement; CDECL;
function Alt_PDE_Get_TotalCustomers(elem: TPDElement): Integer; CDECL;
function Alt_PDE_Get_FromTerminal(elem: TPDElement): Integer; CDECL;
function Alt_PDE_Get_TotalMiles(elem: TPDElement): Double; CDECL;
function Alt_PDE_Get_SectionID(elem: TPDElement): Integer; CDECL;
// function Alt_PDE_Get_MaxCurrent(elem: TPDElement; const AllNodes: TAltAPIBoolean): Double; CDECL; -- removed in favour of the CE version
function Alt_PDE_Get_pctNorm(elem: TPDElement; const AllNodes: TAltAPIBoolean): Double; CDECL;
function Alt_PDE_Get_pctEmerg(elem: TPDElement; const AllNodes: TAltAPIBoolean): Double; CDECL;
// procedure Alt_PDEBatch_Get_MaxCurrent(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const AllNodes: TAltAPIBoolean); CDECL;
procedure Alt_PDEBatch_Get_pctNorm(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const AllNodes: TAltAPIBoolean); CDECL;
procedure Alt_PDEBatch_Get_pctEmerg(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const AllNodes: TAltAPIBoolean); CDECL;

//LoadShape
procedure Alt_LoadShape_Set_Points(elem: TLoadshapeObj; Npts: TAPISize; HoursPtr: Pointer; PMultPtr: Pointer; QMultPtr: Pointer; ExternalMemory: TAltAPIBoolean; IsFloat32: TAltAPIBoolean; Stride: Integer); CDECL;
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
procedure Alt_Transformer_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TControlledTransformerObj; winding: Integer); CDECL;
procedure Alt_Transformer_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TControlledTransformerObj); CDECL;
procedure Alt_Transformer_Get_LossesByType(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TControlledTransformerObj); CDECL;
//EnergyMeter, general
function Alt_Meter_Get_TotalCustomers(elem: TEnergyMeterObj): Integer; CDECL;
procedure Alt_Meter_Get_CalcCurrent(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Set_CalcCurrent(elem: TEnergyMeterObj; ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Alt_Meter_Get_AllocFactors(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
procedure Alt_Meter_Set_AllocFactors(elem: TEnergyMeterObj; ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Alt_Meter_DoReliabilityCalc(elem: TEnergyMeterObj; AssumeRestoration: TAltAPIBoolean); CDECL;
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
//Bus
function Alt_Bus_Get_Name(DSS: TDSSContext; pBus: TDSSBus): PAnsiChar; CDECL;
function Alt_Bus_Get_NumNodes(DSS: TDSSContext; pBus: TDSSBus): Integer; CDECL;
function Alt_Bus_Get_kVBase(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_CoordDefined(DSS: TDSSContext; pBus: TDSSBus): TAltAPIBoolean; CDECL;
function Alt_Bus_Get_X(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
procedure Alt_Bus_Set_X(DSS: TDSSContext; pBus: TDSSBus; value: Double); CDECL;
function Alt_Bus_Get_Y(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
procedure Alt_Bus_Set_Y(DSS: TDSSContext; pBus: TDSSBus; value: Double); CDECL;
function Alt_Bus_Get_Distance(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_IntDuration(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_Lambda(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_CustDuration(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_CustInterrupts(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_NumCustomers(DSS: TDSSContext; pBus: TDSSBus): Integer; CDECL;
function Alt_Bus_Get_NumInterrupts(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_TotalMiles(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
function Alt_Bus_Get_SectionID(DSS: TDSSContext; pBus: TDSSBus): Integer; CDECL;
function Alt_Bus_ZscRefresh(DSS: TDSSContext; pBus: TDSSBus): TAltAPIBoolean; CDECL;
function Alt_Bus_GetUniqueNodeNumber(DSS: TDSSContext; pBus: TDSSBus; StartNumber: Integer): Integer; CDECL;
procedure Alt_Bus_Get_Voltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Nodes(DSS: TDSSContext; var ResultPtr: PInteger; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_SeqVoltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Isc(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Voc(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_puVoltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Zsc0(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Zsc1(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_ZscMatrix(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_YscMatrix(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_ComplexSeqVoltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_puVLL(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_VLL(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_puVMagAngle(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_VMagAngle(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Zsc012Matrix(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Lines(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_Loads(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_PCElements(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
procedure Alt_Bus_Get_PDElements(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
function Alt_Bus_GetListPtr(DSS: TDSSContext): PPointer; CDECL;
function Alt_Bus_GetByIndex(DSS: TDSSContext; idx: Integer): TDSSBus; CDECL;
function Alt_Bus_GetByName(DSS: TDSSContext; name: PAnsiChar): TDSSBus; CDECL;
function Alt_Bus_ToJSON(DSS: TDSSContext; pBus: TDSSBus; joptions: Integer): PAnsiChar; CDECL;
procedure Alt_BusBatch_GetFloat64FromFunc(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; batch: PDSSBus; batchSize: Integer; func: dss_ctx_bus_float64_function_t); CDECL;
procedure Alt_BusBatch_GetInt32FromFunc(DSS: TDSSContext; var ResultPtr: PInteger; ResultCount: PAPISize; batch: PDSSBus; batchSize: Integer; func: dss_ctx_bus_int32_function_t); CDECL;
function Alt_BusBatch_ToJSON(DSS: TDSSContext; batch: PDSSBus; batchSize: Integer; joptions: Integer): PAnsiChar; CDECL;

procedure _Alt_CEBatch_Get_AllxSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; magnitude: boolean);
procedure _Alt_CEBatch_Get_AllCurrentsVoltages_x(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const What: Integer);
procedure _Alt_PDEBatch_Get_x(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const What: integer; const AllNodes: Boolean);

// Used in CAPI_Obj
function alt_Bus_ToJSON_(DSS: TDSSContext; bus: TDSSBus; joptions: Integer): TJSONObject;

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
    Solution,
    PDClass,
    DSSPointerList,
    XYCurve,
    ArrayDef,
    Math,
    Classes,
    CktElementClass,
    CktTree,
    PCClass,
    Ucmatrix,
    ExecHelper;

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
    VPh: Complex3;
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

        Phase2SymComp(@Vph, PComplex3(@V012[iV]));   // Compute Symmetrical components
        iV += 3;
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
    value: PPAnsiCharArray0;
    i: Integer;
    Count: Integer;
begin
    value := PPAnsiCharArray0(ValuePtr);
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
        elem.SetBus(i, value[i - 1]);
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

    NodeV := elem.ActiveCircuit.Solution.NodeV;
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
procedure Alt_CE_Get_SeqPowers_(
    var cBuffer: ArrayOfComplex; 
    NodeV: pNodeVArray; 
    Result: PComplex;
    elem: TDSSCktElement;
    VPh, V012: Complex3;
    IPh, I012: Complex3;
    var nextPos: Integer
);
var
    Nvalues, i, j, k, n, icount: Integer;
    S: Complex;
begin
    NValues := 3 * elem.NTerms;
    nextPos := NValues;
    if (not elem.Enabled) or (elem.NodeRef = NIL) then
        Exit;

    if Length(cBuffer) < elem.Yorder then
        SetLength(cBuffer, elem.Yorder);
    elem.GetCurrents(cBuffer);
    
    if elem.NPhases <> 3 then
    begin
        if (elem.Nphases = 1) and elem.DSS.ActiveCircuit.PositiveSequence then
        begin
            iCount := 1;  // Start with kVA1
            // Put only phase 1 quantities in Pos seq
            for j := 1 to elem.NTerms do
            begin
                k := (j - 1) * elem.NConds;
                n := elem.NodeRef[k + 1];
                Result[icount] := (NodeV[n] * cong(cBuffer[k])) * 0.003; // 3-phase kVA conversion
                inc(icount, 3);
            end;
        end
        else
            for i := 0 to 3 * elem.NTerms - 1 do
                Result[i] := cmplx(-1.0, -1.0);  // Signify n/A
        Exit;
    end;

    icount := 0;
    for j := 1 to elem.NTerms do
    begin
        k := (j - 1) * elem.NConds;
        for i := 1 to 3 do
            Vph[i] := NodeV[elem.NodeRef[i + k]];
        for i := 1 to 3 do
            Iph[i] := cBuffer[k + i - 1];
        Phase2SymComp(@Iph, @I012);
        Phase2SymComp(@Vph, @V012);
        for i := 1 to 3 do
        begin
            S := V012[i] * cong(I012[i]);
            
            // One of the absurd features of FPC: ((S * 0.003).re) is different from (S.re * 0.003)

            // 3-phase kVA conversion
            Result[icount].re := S.re * 0.003;
            Result[icount].im := S.im * 0.003; 
            inc(icount);
        end;
    end;
end;
procedure Alt_CE_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
// All seq Powers of active 3-phase circuit element
// returns kW + j kvar
var
    Nvalues: Integer;
    VPh, V012: Complex3;
    IPh, I012: Complex3;
    cBuffer: ArrayOfComplex = NIL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if MissingSolution(elem) or (elem.NodeRef = NIL) then // or (not elem.Enabled)
        Exit;

    SetLength(cBuffer, 4 * 3);
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * elem.NTerms, 3, elem.NTerms); // allocate for kW and kvar
    Alt_CE_Get_SeqPowers_(
        cBuffer, 
        elem.DSS.ActiveCircuit.Solution.NodeV,
        PComplex(ResultPtr),
        elem,
        Vph, V012,
        IPh, I012,
        Nvalues
    );
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
function Alt_CE_IsOpen(elem: TDSSCktElement; Term, Phs: Integer): TAltAPIBoolean; CDECL;
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
procedure Alt_CE_Get_YPrim(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TDSSCktElement); CDECL;
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
function Alt_CE_Get_HasVoltControl(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
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
function Alt_CE_Get_HasSwitchControl(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
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
function Alt_CE_Get_HasOCPDevice(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
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
function Alt_CE_Get_IsIsolated(elem: TDSSCktElement): TAltAPIBoolean; CDECL;
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
            buffer[j - 1] += cBuffer[i];
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
var
    i, k: Integer;
    CurrMag: Double;
    // MaxPhase: Integer;
    minTerm, maxTerm: Integer;
begin
    Result := 0.0;
    if (not obj.Enabled) or (obj.NodeRef = NIL) then
        Exit;

    if terminalIdx = -1 then
    begin
        minTerm := 1;
        maxTerm := obj.NTerms;
    end
    else
    begin
        if (terminalIdx = 0) or (terminalIdx > obj.NTerms) then
        begin
            obj.DoSimpleMsg('Invalid terminal index (%d) provided for "%s". Element has %d terminals. Use -1 for all terminals.', [obj.NTerms, obj.FullName], 97803);
            Exit;
        end;
        minTerm := terminalIdx;
        maxTerm := terminalIdx;
    end;

    obj.ComputeIterminal();
    // Method: Get max current at terminal (magnitude)
    for terminalIdx := minTerm to maxTerm do
    begin
        k := (terminalIdx - 1) * obj.NConds; // starting index of terminal
        for i := 1 to obj.Fnphases do
        begin
            CurrMag := Cabs(obj.Iterminal[k + i]);
            if CurrMag > Result then
            begin
                Result := CurrMag;
            end;
        end;
    end;
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
procedure Alt_PCE_Set_VariableValue(elem: TPCElement; varIdx: Integer; value: Double); CDECL;
begin
    if (varIdx <= 0) or (varIdx > elem.NumVariables) then
    begin
        if DSS_CAPI_EXT_ERRORS then
            DoSimpleMsg(elem.DSS, 'Invalid variable index %d for "%s"', [varIdx, elem.FullName], 100002);
        Exit;
    end;
    elem.Variable[varIdx] := value;
end;
//------------------------------------------------------------------------------
function Alt_PCE_Get_VariableSValue(elem: TPCElement; varName: PAnsiChar): Double; CDECL;
var
    sname: String;
    varIdx: Integer;
begin
    Result := 0;
    sname := varName;
    varIdx := elem.LookupVariable(sname);
    if (varIdx <= 0) or (varIdx > elem.NumVariables) then
    begin
        DoSimpleMsg(elem.DSS, 'Invalid variable name %s for "%s"', [sname, elem.FullName], 100002);
        Exit;
    end;
    Result := elem.Variable[varIdx];
end;
//------------------------------------------------------------------------------
procedure Alt_PCE_Set_VariableSValue(elem: TPCElement; varName: PAnsiChar; value: Double); CDECL;
var
    sname: String;
    varIdx: Integer;
begin
    sname := varName;
    varIdx := elem.LookupVariable(sname);
    if (varIdx <= 0) or (varIdx > elem.NumVariables) then
    begin
        DoSimpleMsg(elem.DSS, 'Invalid variable name %s for "%s"', [sname, elem.FullName], 100002);
        Exit;
    end;
    elem.Variable[varIdx] := value;
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_NumPhases(elem: TDSSCktElement): Integer; CDECL;
begin
    Result := elem.NPhases
end;
//------------------------------------------------------------------------------
function Alt_CE_Get_DisplayName(elem: TDSSCktElement): PAnsiChar; CDECL;
begin
    if elem.DisplayName = '' then
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
procedure Alt_CE_Set_DisplayName(elem: TDSSCktElement; const value: PAnsiChar); CDECL;
begin
    elem.DisplayName := value;
end;
//------------------------------------------------------------------------------
function Alt_PDE_Get_IsShunt(elem: TPDElement): TAltAPIBoolean; CDECL;
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
procedure Alt_LoadShape_Set_Points(elem: TLoadshapeObj; Npts: TAPISize; HoursPtr: Pointer; PMultPtr: Pointer; QMultPtr: Pointer; ExternalMemory: TAltAPIBoolean; IsFloat32: TAltAPIBoolean; Stride: Integer); CDECL;
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
    Alt_Monitor_Get_dblHourFreq(pmon, true, ResultPtr, ResultCount);
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
    if pmon.RecordSize <= 0 then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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
procedure Alt_Transformer_Get_WdgVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TControlledTransformerObj; winding: Integer); CDECL;
begin
    if (winding > 0) and (winding <= elem.NumWindings) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * elem.nphases);
        if elem.Enabled then
            elem.GetWindingVoltages(winding, pComplexArray(ResultPtr));
        Exit;
    end;
    elem.DoSimpleMsg('Invalid winding number (%d) for transformer %s. Valid numbers: from 1 to %d.', [winding, elem.FullName, elem.NumWindings], 8986);
    DefaultResult(ResultPtr, ResultCount);
end;
//------------------------------------------------------------------------------
procedure Alt_Transformer_Get_WdgCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TControlledTransformerObj); CDECL;
var
    NumCurrents: Integer;
begin
    NumCurrents := 2 * elem.NPhases * elem.NumWindings; // 2 currents per winding
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NumCurrents);
    if elem.Enabled then
        elem.GetAllWindingCurrents(pComplexArray(ResultPtr));
end;
//------------------------------------------------------------------------------
procedure Alt_Transformer_Get_LossesByType(var ResultPtr: PDouble; ResultCount: PAPISize; elem: TControlledTransformerObj); CDECL;
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
    value: PDoubleArray0;
    i: Integer;
begin
    if ValueCount <> elem.NPhases then
    begin
        elem.DoSimpleMsg(_('The provided number of values does not match the element''s number of phases.'), 5025);
        Exit;
    end;
    value := PDoubleArray0(ValuePtr);
    for i := 1 to elem.NPhases do
        elem.CalculatedCurrent[i] := value[i - 1];   // Just set the real part
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
    value: PDoubleArray0;
    i: Integer;
begin
    value := PDoubleArray0(ValuePtr);
    if ValueCount <> elem.NPhases then
    begin
        elem.DoSimpleMsg(_('The provided number of values does not match the element''s number of phases.'), 5026);
        Exit;
    end;
    for i := 1 to elem.NPhases do
    begin
        elem.PhsAllocationFactor[i] := value[i - 1];
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_DoReliabilityCalc(elem: TEnergyMeterObj; AssumeRestoration: TAltAPIBoolean); CDECL;
begin
    elem.AssumeRestoration := AssumeRestoration;
    elem.CalcReliabilityIndices();
end;
//------------------------------------------------------------------------------
procedure Alt_Meter_Get_ZonePCEs(var ResultPtr: PPointer; ResultCount: PAPISize; elem: TEnergyMeterObj); CDECL;
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
procedure Alt_CEBatch_Get_Losses(var resultPtr: PDouble; resultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: TAPISize); CDECL;
var
    Result: PDoubleArray0;
    CResultPtr: pComplex;
    i: TAPISize;
begin
    resultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;
    if MissingSolution(batch^) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * batchSize);
    CResultPtr := pComplex(ResultPtr);
    for i := 0 to batchSize do
    begin
        if batch^ <> NIL then
            CResultPtr^ := batch^.Losses;

        Inc(batch);
        Inc(CResultPtr);
    end;

    i := 0;
    while i < 2 * batchSize do
    begin
        Result[i] := Result[i] * 0.001;
        Inc(i);
    end;
end;
//------------------------------------------------------------------------------
procedure Alt_CEBatch_Get_PhaseLosses(var resultPtr: PDouble; resultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: TAPISize); CDECL;
// Returns Phase losses in kW, kVar
var
    Result: PDoubleArray0;
    NValuesTotal, NValues, i: Integer;
    pElem: TDSSCktElementPtr;
    outPtr: PComplex;
begin
    resultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) then
        Exit;
    if MissingSolution(batch^) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    NValuesTotal := 0;
    pElem := batch;    
    for i := 1 to batchSize do
    begin
        NValuesTotal += pElem^.NPhases;
        Inc(pElem);
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValuesTotal);
    outPtr := PComplex(ResultPtr);
    pElem := batch;    
    for i := 1 to batchSize do
    begin
        NValues := pElem^.NPhases;
        pElem^.GetPhaseLosses(NValues, pComplexArray(outPtr));
        inc(outPtr, NValues);
        inc(pElem);
    end;

    for i := 0 to (2 * NValuesTotal - 1) do
    begin
        Result[i] *= 0.001;
    end;
end;
//------------------------------------------------------------------------------
function Alt_Bus_Get_Name(DSS: TDSSContext; pBus: TDSSBus): PAnsiChar; CDECL;
begin
    Result := PChar(pBus.Name);
end;
function Alt_Bus_Get_NumNodes(DSS: TDSSContext; pBus: TDSSBus): Integer; CDECL;
begin
    Result := pBus.NumNodesThisBus;
end;
function Alt_Bus_Get_kVBase(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.kVBase;
end;
function Alt_Bus_Get_CoordDefined(DSS: TDSSContext; pBus: TDSSBus): TAltAPIBoolean; CDECL;
begin
    Result := pBus.CoordDefined;
end;
function Alt_Bus_Get_X(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := 0;
    if pBus.CoordDefined then
        Result := pBus.x;
end;
procedure Alt_Bus_Set_X(DSS: TDSSContext; pBus: TDSSBus; value: Double); CDECL;
begin
    pBus.CoordDefined := true;
    pBus.x := value;
end;
function Alt_Bus_Get_Y(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := 0;
    if pBus.CoordDefined then
        Result := pBus.y;
end;
procedure Alt_Bus_Set_Y(DSS: TDSSContext; pBus: TDSSBus; value: Double); CDECL;
begin
    pBus.CoordDefined := true;
    pBus.y := value;
end;
function Alt_Bus_Get_Distance(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.DistFromMeter;
end;
function Alt_Bus_Get_IntDuration(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.Bus_Int_Duration;
end;
function Alt_Bus_Get_Lambda(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.BusFltRate;
end;
function Alt_Bus_Get_CustDuration(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.BusCustDurations;
end;
function Alt_Bus_Get_CustInterrupts(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.BusCustInterrupts;
end;
function Alt_Bus_Get_NumCustomers(DSS: TDSSContext; pBus: TDSSBus): Integer; CDECL;
begin
    Result := pBus.BusTotalNumCustomers;
end;
function Alt_Bus_Get_NumInterrupts(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.Bus_Num_Interrupt;
end;
function Alt_Bus_Get_TotalMiles(DSS: TDSSContext; pBus: TDSSBus): Double; CDECL;
begin
    Result := pBus.BusTotalMiles;
end;
function Alt_Bus_Get_SectionID(DSS: TDSSContext; pBus: TDSSBus): Integer; CDECL;
begin
    Result := pBus.BusSectionID;
end;
function Alt_Bus_ZscRefresh(DSS: TDSSContext; pBus: TDSSBus): TAltAPIBoolean; CDECL;
begin
    Result := (DSS.DSSExecutive.DoZscRefresh(pBus) = 0);
end;
function Alt_Bus_GetUniqueNodeNumber(DSS: TDSSContext; pBus: TDSSBus; StartNumber: Integer): Integer; CDECL;
begin
    Result := DSS.ActiveCircuit.GetUniqueNodeNumber(pBus, StartNumber);
end;

procedure Alt_Bus_Get_Voltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
// Return Complex for all nodes of voltages for Active Bus
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: Complex;
begin
    Nvalues := pBus.NumNodesThisBus;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;
    jj := 1;
    for i := 1 to NValues do
    begin
        // this code so nodes come out in order from smallest to larges
        repeat
            NodeIdx := pBus.FindIdx(jj);  // Get the index of the Node that matches jj
            inc(jj)
        until NodeIdx > 0;

        Volts := DSS.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)];
        Result[iV] := Volts.re;
        Inc(iV);
        Result[iV] := Volts.im;
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_Nodes(DSS: TDSSContext; var ResultPtr: PInteger; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
// return array of node numbers corresponding to voltages
var
    Result: PIntegerArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
begin
    Nvalues := pBus.NumNodesThisBus;
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, NValues);
    iV := 0;
    jj := 1;
    for i := 1 to NValues do
    begin
        // this code so nodes come out in order from smallest to larges
        repeat
            NodeIdx := pBus.FindIdx(jj);  // Get the index of the Node that matches jj
            inc(jj)
        until NodeIdx > 0;
        Result[iV] := pBus.GetNum(NodeIdx);
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_SeqVoltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
// Compute sequence voltages for Active Bus
// magnitude only
// returns a set of seq voltages (3)
var
    Result: PDoubleArray0;
    Nvalues, i, iV: Integer;
    VPh, V012: Complex3;
begin
    Nvalues := pBus.NumNodesThisBus;
    if Nvalues > 3 then
        Nvalues := 3;

    // Assume nodes 1, 2, and 3 are the 3 phases
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 3);
    if Nvalues <> 3 then
    begin
        for i := 1 to 3 do
            Result[i - 1] := -1.0;  // Signify seq voltages n/A for less then 3 phases
        Exit;
    end;

    iV := 0;
    for i := 1 to 3 do
    begin
        Vph[i] := DSS.ActiveCircuit.Solution.NodeV[pBus.Find(i)];
    end;

    Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

    for i := 1 to 3 do  // Stuff it in the result
    begin
        Result[iV] := Cabs(V012[i]);
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_Isc(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Isc: Complex;
    i, iV, NValues: Integer;
begin
    if pBus.BusCurrent = NIL then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    NValues := pBus.NumNodesThisBus;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;
    for i := 1 to NValues do
    begin
        Isc := pBus.BusCurrent[i];
        Result[iV] := Isc.Re;
        Inc(iV);
        Result[iV] := Isc.Im;
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_Voc(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Voc: Complex;
    i, iV, NValues: Integer;
begin
    if pBus.VBus = NIL then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    NValues := pBus.NumNodesThisBus;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;
    for i := 1 to NValues do
    begin
        Voc := pBus.VBus[i];
        Result[iV] := Voc.Re;
        Inc(iV);
        Result[iV] := Voc.Im;
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_puVoltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: Complex;
    BaseFactor: Double;
begin
    Nvalues := pBus.NumNodesThisBus;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;
    jj := 1;
    if pBus.kVBase > 0.0 then
        BaseFactor := 1000.0 * pBus.kVBase
    else
        BaseFactor := 1.0;

    for i := 1 to NValues do
    begin
        // this code so nodes come out in order from smallest to larges
        repeat
            NodeIdx := pBus.FindIdx(jj);  // Get the index of the Node that matches jj
            inc(jj)
        until NodeIdx > 0;

        Volts := DSS.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)];
        Result[iV] := Volts.re / BaseFactor;
        Inc(iV);
        Result[iV] := Volts.im / BaseFactor;
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_Zsc0(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Z: Complex;
begin
    Z := pBus.Zsc0;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := Z.Re;
    Result[1] := Z.Im;
end;

procedure Alt_Bus_Get_Zsc1(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Z: Complex;
begin
    Z := pBus.Zsc1;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
    Result[0] := Z.Re;
    Result[1] := Z.Im;
end;

procedure Alt_Bus_Get_ZscMatrix(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nelements, iV, i, j: Integer;
    Z: Complex;
begin
    DefaultResult(ResultPtr, ResultCount);
    try
        if pBus.Zsc = NIL then
            Exit;

        Nelements := pBus.Zsc.Order;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * Nelements * Nelements, Nelements, Nelements);
        iV := 0;
        for i := 1 to Nelements do
        begin
            for j := 1 to Nelements do
            begin
                Z := pBus.Zsc[i, j];
                Result[iV] := Z.Re;
                Inc(iV);
                Result[iV] := Z.Im;
                Inc(iV);
            end;
        end;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'ZscMatrix Error: %s', [E.message], 5016);
    end;
end;

procedure Alt_Bus_Get_YscMatrix(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nelements, iV, i, j: Integer;
    Y1: Complex;
begin
    DefaultResult(ResultPtr, ResultCount);
    try
        if pBus.Ysc = NIL then
            Exit;

        Nelements := pBus.Ysc.Order;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * Nelements * Nelements, Nelements, Nelements);
        iV := 0;
        for i := 1 to Nelements do
        begin
            for j := 1 to Nelements do
            begin
                Y1 := pBus.Ysc[i, j];
                Result[iV] := Y1.Re;
                Inc(iV);
                Result[iV] := Y1.Im;
                Inc(iV);
            end;
        end;
    except
        On E: Exception do
            DoSimpleMsg(DSS, 'ZscMatrix Error: %s', [E.message], 5017);
    end;
end;

procedure Alt_Bus_Get_ComplexSeqVoltages(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV: Integer;
    VPh, V012: Complex3;
begin
    Nvalues := pBus.NumNodesThisBus;
    if Nvalues > 3 then
        Nvalues := 3;

    // Assume nodes labelled 1, 2, and 3 are the 3 phases
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 6);
    if Nvalues <> 3 then
        for i := 1 to 6 do
            Result[i - 1] := -1.0  // Signify seq voltages n/A for less then 3 phases
    else
    begin
        iV := 0;
        for i := 1 to 3 do
        Vph[i] := DSS.ActiveCircuit.Solution.NodeV[pBus.Find(i)];

        Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

        for i := 1 to 3 do  // Stuff it in the result
        begin
            Result[iV] := V012[i].re;
            Inc(iV);
            Result[iV] := V012[i].im;
            Inc(iV);
        end;
    end;
end;

procedure Alt_Bus_Get_puVLL(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdxi, NodeIdxj, jj, k: Integer;
    Volts: Complex;
    BaseFactor: Double;
    NodeV: pNodeVArray;
begin
    NodeV := DSS.ActiveCircuit.Solution.NodeV;
    Nvalues := pBus.NumNodesThisBus;
    if Nvalues > 3 then
        Nvalues := 3;

    if Nvalues <= 1 then
    begin  // for 1-phase buses, do not attempt to compute.
        //TODO: actual error?
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := -99999.0;
        Result[1] := 0.0;
        Exit;
    end;
    
    if Nvalues = 2 then
        Nvalues := 1;  // only one L-L voltage if 2 phase
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;

    if pBus.kVBase > 0.0 then
        BaseFactor := 1000.0 * pBus.kVBase * sqrt3
    else
        BaseFactor := 1.0;

    for i := 1 to NValues do     // for 2- or 3-phases
    begin
        // this code assumes the nodes are ordered 1, 2, 3
        //------------------------------------------------------------------------------------------------
        // This section was added to prevent measuring using disconnected nodes, for example, if the
        // bus has 2 nodes but those are 1 and 3, that will bring a problem.
        jj := i;
        repeat
            NodeIdxi := pBus.FindIdx(jj);  // Get the index of the Node that matches i
            inc(jj);
        until NodeIdxi > 0;

        // (2020-03-01) Changed in DSS C-API to avoid some corner
        // cases that resulted in infinite loops
        for k := 1 to 3 do
        begin
            NodeIdxj := pBus.FindIdx(jj);  // Get the index of the Node that matches i
            if jj > 3 then
                jj := 1
            else
                inc(jj);

            if NodeIdxj > 0 then
                break;
        end;
        if NodeIdxj = 0 then
        begin
            // Could not find appropriate node
            DefaultResult(ResultPtr, ResultCount);
            Exit;
        end;
        //------------------------------------------------------------------------------------------------
            Volts := NodeV[pBus.GetRef(NodeIdxi)] - NodeV[pBus.GetRef(NodeIdxj)];
        Result[iV] := Volts.re / BaseFactor;
        Inc(iV);
        Result[iV] := Volts.im / BaseFactor;
        Inc(iV);
    end;
end;


procedure Alt_Bus_Get_VLL(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdxi, NodeIdxj, jj, k: Integer;
    Volts: Complex;
    NodeV: pNodeVArray;
begin
    NodeV := DSS.ActiveCircuit.Solution.NodeV;
    Nvalues := pBus.NumNodesThisBus;
    if Nvalues > 3 then
        Nvalues := 3;

    if Nvalues <= 1 then
    begin  // for 1-phase buses, do not attempt to compute.
        //TODO: actual error?
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := -99999.0;
        Result[1] := 0.0;
        Exit;
    end;

    if Nvalues = 2 then
        Nvalues := 1;  // only one L-L voltage if 2 phase
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;
    
    for i := 1 to NValues do     // for 2- or 3-phases
    begin
        // this code assumes the nodes are ordered 1, 2, 3
        //------------------------------------------------------------------------------------------------
        // This section was added to prevent measuring using disconnected nodes, for example, if the
        // bus has 2 nodes but those are 1 and 3, that will bring a problem.

        jj := i;
        repeat
            NodeIdxi := pBus.FindIdx(jj);  // Get the index of the Node that matches i
            inc(jj);
        until NodeIdxi > 0;

        // (2020-03-01) Changed in DSS C-API to avoid some corner
        // cases that resulted in infinite loops
        for k := 1 to 3 do
        begin
            NodeIdxj := pBus.FindIdx(jj);  // Get the index of the Node that matches i
            if jj > 3 then
                jj := 1
            else
                inc(jj);

            if NodeIdxj > 0 then
                break;
        end;
        if NodeIdxj = 0 then
        begin
            // Could not find appropriate node
            DefaultResult(ResultPtr, ResultCount);
            Exit;
        end;
        //------------------------------------------------------------------------------------------------
        Volts := NodeV[pBus.GetRef(NodeIdxi)] - NodeV[pBus.GetRef(NodeIdxj)];
        Result[iV] := Volts.re;
        Inc(iV);
        Result[iV] := Volts.im;
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_puVMagAngle(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: polar;
    Basefactor: Double;
begin
    Nvalues := pBus.NumNodesThisBus;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;
    jj := 1;

    if pBus.kVBase > 0.0 then
        BaseFactor := 1000.0 * pBus.kVBase
    else
        BaseFactor := 1.0;

    for i := 1 to NValues do
    begin
        // this code so nodes come out in order from smallest to larges
        repeat
            NodeIdx := pBus.FindIdx(jj);  // Get the index of the Node that matches jj
            inc(jj)
        until NodeIdx > 0;

        Volts := ctopolardeg(DSS.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)]);
        Result[iV] := Volts.mag / BaseFactor;
        Inc(iV);
        Result[iV] := Volts.ang;
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_VMagAngle(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: polar;
begin
    Nvalues := pBus.NumNodesThisBus;
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
    iV := 0;
    jj := 1;
    for i := 1 to NValues do
    begin
        // this code so nodes come out in order from smallest to larges
        repeat
            NodeIdx := pBus.FindIdx(jj);  // Get the index of the Node that matches jj
            inc(jj)
        until NodeIdx > 0;

        Volts := ctopolardeg(DSS.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)]);
        Result[iV] := Volts.mag;
        Inc(iV);
        Result[iV] := Volts.ang;
        Inc(iV);
    end;
end;

procedure Alt_Bus_Get_Zsc012Matrix(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; pBus: TDSSBus); CDECL; //TODO: remove duplication between this and DoZsc012Cmd
var
    Zsc012Temp: TCmatrix;
    NValues: Integer;
    Norder: Integer;
begin
    if (pBus.NumNodesThisBus <> 3) or (pBus.Zsc = NIL) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Nvalues := pBus.NumNodesThisBus * pBus.NumNodesThisBus * 2;  // Should be 9 complex numbers
    // Compute ZSC012 for 3-phase buses else leave it zeros
    // ZSC012 = Ap2s Zsc As2p
    Zsc012Temp := pBus.Zsc.MtrxMult(As2p);  // temp for intermediate result
    if Assigned(pBus.ZSC012) then
        pBus.ZSC012.Free;
    pBus.ZSC012 := Ap2s.MtrxMult(Zsc012Temp);
    // Cleanup
    Zsc012Temp.Free;

    // Return all the elements of ZSC012
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NValues, pBus.NumNodesThisBus, pBus.NumNodesThisBus);
    Move(pBus.ZSC012.GetValuesArrayPtr(Norder)[1], ResultPtr[0], NValues * SizeOf(Double));
end;

procedure _Alt_Bus_Get_XElements(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus; loads: Boolean; lines: Boolean; pces: Boolean; pdes: Boolean);
var
    cls: TDSSClass;
    i, n, t, nbus: Integer;
    busName, elemBus, elemBus2: String;
    nodes: Array of Integer = NIL;
    found: Boolean;
    elem: TDSSCktElement;
    maxTerm: Integer = 1;
    Result: PPointerArray0;
begin
    // Initially allocate a buffer for 10 elements.
    Result := DSS_RecreateArray_PPointer(ResultPtr, ResultCount, 10);
    ResultCount[0] := 0;
    SetLength(nodes, pBus.NumNodesThisBus);
    for i := 1 to pBus.NumNodesThisBus do
        nodes[i - 1] := pBus.GetRef(i);

    if pdes or lines then
        maxTerm := 2;

    busName := AnsiLowerCase(pBus.Name);
    for i := 1 to DSS.DSSClassList.Count do
    begin
        cls := DSS.DSSClassList.Get(i);
        if not (cls is TCktElementClass) then
            continue;

        // Checks if this class is included in the targets
        if not (
            (loads and (cls = DSS.LoadClass)) or
            (lines and (cls = DSS.LineClass)) or
            (pces and (cls.ClassType.InheritsFrom(TPCClass) or (cls = DSS.CapacitorClass) or (cls = DSS.ReactorClass))) or
            (pdes and (cls.ClassType.InheritsFrom(TPDClass)))
        ) then
            continue;

        // If it is, checks all the elements to verify if one or more are
        // connected to the bus given
        for elem in cls do
        begin
            if (nodes <> NIL) and (elem.Terminals <> NIL) and (elem.Terminals[0].TermNodeRef <> NIL) then
            begin
                // Fast path
                found := False;
                for t := 0 to Min(High(elem.Terminals), maxTerm) do
                begin
                    for n := 0 to High(elem.Terminals[0].TermNodeRef) do
                    begin
                        for nbus in nodes do
                        begin
                            found := (elem.Terminals[0].TermNodeRef[n] = nbus);
                            if not found then
                                continue;

                            if pdes then
                            begin
                                elemBus := AnsiLowerCase(StripExtension(elem.GetBus(t + 1)));
                                if (t = 0) then
                                    elemBus2 := AnsiLowerCase(StripExtension(elem.GetBus(t + 2)))
                                else
                                    elemBus2 := AnsiLowerCase(StripExtension(elem.GetBus(t)));
                                    
                                if elemBus = elemBus2 then
                                    break;
                            end;
                            Result := DSS_RecreateArray_PPointer(ResultPtr, ResultCount, ResultCount^ + 1, true);
                            Result[ResultCount^ - 1] := elem;
                            break;
                        end;
                        if found then
                            break;
                    end;
                    if found then
                        break;
                end;
                continue;
            end;

            // Original code as fallback
            for t := 0 to Min(High(elem.Terminals), maxTerm) do
            begin
                elemBus := AnsiLowerCase(StripExtension(elem.GetBus(t)));
                if elemBus <> busName then
                    continue;

                if (pdes) then
                begin
                    if (t = 0) then
                        elemBus2 := AnsiLowerCase(StripExtension(elem.GetBus(t + 1)))
                    else
                        elemBus2 := AnsiLowerCase(StripExtension(elem.GetBus(t - 1)));

                    if elemBus = elemBus2 then
                        break;
                end;

                Result := DSS_RecreateArray_PPointer(ResultPtr, ResultCount, ResultCount^ + 1, true);
                Result[ResultCount^ - 1] := elem;
                break;
            end;
        end;
    end;
end;

procedure Alt_Bus_Get_Lines(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
begin
    _Alt_Bus_Get_XElements(DSS, ResultPtr, ResultCount, pBus, false, true, false, false);
end;
procedure Alt_Bus_Get_Loads(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
begin
    _Alt_Bus_Get_XElements(DSS, ResultPtr, ResultCount, pBus, true, false, false, false);
end;
procedure Alt_Bus_Get_PCElements(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
begin
    _Alt_Bus_Get_XElements(DSS, ResultPtr, ResultCount, pBus, false, false, true, false);
end;
procedure Alt_Bus_Get_PDElements(DSS: TDSSContext; var ResultPtr: PPointer; ResultCount: PAPISize; pBus: TDSSBus); CDECL;
begin
    _Alt_Bus_Get_XElements(DSS, ResultPtr, ResultCount, pBus, false, false, false, true);
end;

function Alt_Bus_GetListPtr(DSS: TDSSContext): PPointer; CDECL;
begin
    Result := PPointer(DSS.ActiveCircuit.Buses);
end;

function Alt_Bus_GetByIndex(DSS: TDSSContext; idx: Integer): TDSSBus; CDECL;
begin
    if (idx >= 0) and (idx < DSS.ActiveCircuit.NumBuses) then
    begin
        Result := DSS.ActiveCircuit.Buses[idx + 1];
        Exit;
    end;
    Result := NIL;
    DoSimpleMsg(DSS, 'Could not find bus with index number "%d".', [idx], 8984);
end;

function Alt_Bus_GetByName(DSS: TDSSContext; name: PAnsiChar): TDSSBus; CDECL;
var
    idx: Integer;
    sname: String;
begin
    sname := StripExtension(String(name));
    idx := DSS.ActiveCircuit.BusList.Find(sname);
    if idx = 0 then
    begin
        Result := NIL;
        DoSimpleMsg(DSS, 'Could not find bus named "%s".', [sname], 8985);
        Exit;
    end;
    Result := DSS.ActiveCircuit.Buses[idx];
end;

procedure Alt_BusBatch_GetFloat64FromFunc(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; batch: PDSSBus; batchSize: Integer; func: dss_ctx_bus_float64_function_t); CDECL;
var
    presult: PDouble;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or ((@func) = NIL) then
        Exit;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;

    for i := 1 to batchSize do
    begin
        presult^ := func(DSS, batch^);
        inc(batch);
        inc(presult);
    end;
end;

procedure Alt_BusBatch_GetInt32FromFunc(DSS: TDSSContext; var ResultPtr: PInteger; ResultCount: PAPISize; batch: PDSSBus; batchSize: Integer; func: dss_ctx_bus_int32_function_t); CDECL;
var
    presult: PInteger;
    i: Integer;
begin
    ResultCount[0] := 0;
    if (batch = NIL) or (batch^ = NIL) or ((@func) = NIL) then
    begin
        Exit;
    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, batchSize);
    presult := ResultPtr;
    for i := 1 to batchSize do
    begin
        presult^ := func(DSS, batch^);
        inc(batch);
        inc(presult);
    end;
end;

function alt_Bus_ToJSON_(DSS: TDSSContext; bus: TDSSBus; joptions: Integer): TJSONObject;
begin
    Result := TJSONObject.Create(['Name', bus.Name]);
    if bus.CoordDefined then
    begin
        Result.Add('X', bus.x);
        Result.Add('Y', bus.y);
    end;
    if bus.kVBase <> 0 then
        Result.Add('kVLN', bus.kVBase);
    if bus.Keep then
        Result.Add('Keep', true);
end;

function Alt_Bus_ToJSON(DSS: TDSSContext; pBus: TDSSBus; joptions: Integer): PAnsiChar; CDECL;
var
    json: TJSONObject = NIL;
begin
    Result := NIL;
    try
        json := alt_Bus_ToJSON_(DSS, pBus, joptions);
        if (Integer(DSSJSONOptions.Pretty) and joptions) <> 0 then
            Result := DSS_CopyStringAsPChar(json.FormatJSON([], 2))
        else
            Result := DSS_CopyStringAsPChar(json.FormatJSON([foSingleLineArray, foSingleLineObject, foskipWhiteSpace], 0));
    except
        on E: Exception do
            DoSimpleMsg(DSS, 'Error converting bus data to JSON: %s', [E.message], 5020);
    end;
    FreeAndNil(json);
end;

function Alt_BusBatch_ToJSON(DSS: TDSSContext; batch: PDSSBus; batchSize: Integer; joptions: Integer): PAnsiChar; CDECL;
var
    json: TJSONArray = NIL;
    i: Integer;
begin
    Result := NIL;
    if (batch = NIL) or (batch^ = NIL) then
        Exit;

    try
        json := TJSONArray.Create();
        for i := 1 to batchSize do
        begin
            json.Add(alt_Bus_ToJSON_(DSS, TDSSBus(batch^), joptions));
            inc(batch);
        end;
        if (Integer(DSSJSONOptions.Pretty) and joptions) <> 0 then
            Result := DSS_CopyStringAsPChar(json.FormatJSON([], 2))
        else
            Result := DSS_CopyStringAsPChar(json.FormatJSON([foSingleLineArray, foSingleLineObject, foskipWhiteSpace], 0));
    except
        on E: Exception do
            DoSimpleMsg(DSS, 'Error converting bus data to JSON: %s', [E.message], 5020);
    end;
    FreeAndNil(json);
end;
//------------------------------------------------------------------------------
procedure Alt_CEBatch_Get_TotalPowers(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
var
    cBuffer: pComplexArray;
    myInit, myEnd, maxSize, NTermsTotal, idx, j, i, iV: Integer;
    buffer: Array of Complex;
    Result: PDoubleArray0;
    pElem: TDSSCktElementPtr;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or MissingSolution(TDSSCktElement(batch^)) then
    begin
        ResultCount[0] := 0;
        Exit;
    end;

    NTermsTotal := 0;
    maxSize := 0;
    pElem := TDSSCktElementPtr(batch);
    for idx := 1 to batchSize do
    begin
        Inc(NTermsTotal, pElem^.NTerms);
        maxSize := max(maxSize, pElem^.NConds * pElem^.Nterms);
        inc(pElem);
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NTermsTotal);
    cBuffer := Allocmem(2 * SizeOf(Double) * maxSize);
    SetLength(buffer, maxSize);
    iV := 0;
    pElem := TDSSCktElementPtr(batch);
    dec(pElem);
    for idx := 1 to batchSize do
    begin
        inc(pElem);
        if (not pElem^.Enabled) or (pElem^.NodeRef = NIL) then
        begin
            Inc(iV, 2 * pElem^.NTerms);
            continue
        end;
        pElem^.GetPhasePower(cBuffer);    
        for j := 1 to pElem^.Nterms do
        Begin
            buffer[j - 1] := 0;
            myInit := (j - 1) * pElem^.NConds + 1;
            myEnd := pElem^.NConds * j;
            for i := myInit to myEnd do
            begin
                buffer[j - 1] += cBuffer[i];
            end;
            Result[iV + 0] := buffer[j - 1].re * 0.001;
            Result[iV + 1] := buffer[j - 1].im * 0.001; 
            Inc(iV, 2);
        end;
    end;
    Reallocmem(cBuffer, 0);
end;

procedure Alt_CEBatch_Get_Powers(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
// Return complex kW, kvar in each conductor for each terminal, for each element in the batch
var
    Result: PDoubleArray0;
    NValuesTotal, NValues, i: Integer;
    pElem: TDSSCktElementPtr;
    CResultPtr: PComplex;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or MissingSolution(TDSSCktElement(batch^)) then
    begin
        ResultCount[0] := 0;
        Exit;
    end;

    // Get the total number of (complex) elements
    NValuesTotal := 0;
    pElem := TDSSCktElementPtr(batch);
    for i := 1 to batchSize do
    begin
        Inc(NValuesTotal, pElem^.NConds * pElem^.NTerms);
        inc(pElem);
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NValuesTotal * 2); // TODO: dimensions
    CResultPtr := PComplex(ResultPtr);

    // Get the actual values
    pElem := TDSSCktElementPtr(batch);
    for i := 1 to batchSize do
    begin
        NValues := pElem^.NConds * pElem^.NTerms;
        
        if pElem^.Enabled then
            pElem^.GetPhasePower(pComplexArray(CResultPtr));
            
        Inc(CResultPtr, NValues);
        inc(pElem);
    end;
    for i := 0 to (2 * NValuesTotal) - 1 do
        Result[i] *= 0.001;    
end;

procedure Alt_CEBatch_Get_SeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
// Return complex seq kW, kvar for all terminals of all elements in the batch
var
    CResultPtr: PComplex;
    pElem: TDSSCktElementPtr;
    i, NtermsTotal, Next: Integer;
    VPh, V012: Complex3;
    IPh, I012: Complex3;
    cBuffer: ArrayOfComplex = NIL;
    NodeV: pNodeVArray; 
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or MissingSolution(TDSSCktElement(batch^)) then
    begin
        ResultCount[0] := 0;
        Exit;
    end;

    SetLength(cBuffer, 4 * 3);
    // Get the total number of (complex) elements
    NtermsTotal := 0;
    pElem := TDSSCktElementPtr(batch);
    for i := 1 to batchSize do
    begin
        NtermsTotal += pElem^.NTerms;
        inc(pElem);
    end;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * NtermsTotal, 3, NtermsTotal); // allocate for kW and kvar
    CResultPtr := PComplex(ResultPtr);
    // Get the actual values
    pElem := TDSSCktElementPtr(batch);
    NodeV := pElem^.DSS.ActiveCircuit.Solution.NodeV;
    for i := 1 to batchSize do
    begin
        Alt_CE_Get_SeqPowers_(
            cBuffer, 
            NodeV,
            CResultPtr,
            pElem^,
            Vph, V012,
            IPh, I012,
            Next
        );
        Inc(CResultPtr, Next);
        inc(pElem);
    end;
end;

procedure _Alt_CEBatch_Get_AllxSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; magnitude: boolean);
type
    PPolar = ^Polar;
var
    Result: PDoubleArray0;
    pElem: TDSSCktElementPtr;
    cBuffer: pComplexArray;
    i012v, i012: pComplex;
    maxSize, NTermsTotal, i, j, k, idx: Integer;
    posSeq: Boolean;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or MissingSolution(TDSSCktElement(batch^)) then
    begin
        ResultCount[0] := 0;
        Exit;
    end;
    // Get the total number of (complex) elements, max. terminals, max. conductors
    NTermsTotal := 0;
    pElem := TDSSCktElementPtr(batch);
    posSeq := pElem^.ActiveCircuit.PositiveSequence;
    maxSize := 0;
    for idx := 1 to batchSize do
    begin
        Inc(NTermsTotal, pElem^.NTerms);
        maxSize := max(maxSize, pElem^.Yorder);
        inc(pElem);
    end;

    // Get the actual values
    i012v := AllocMem(SizeOf(Complex) * 3 * NTermsTotal);
    i012 := i012v; // this is a running pointer
    cBuffer := AllocMem(SizeOf(Complex) * maxSize);
    pElem := TDSSCktElementPtr(batch);
    dec(pElem);
    for idx := 1 to batchSize do
    begin
        inc(pElem);
        if pElem^.Enabled then
            pElem^.GetCurrents(cBuffer)
        else
            FillByte(cBuffer^, SizeOf(Complex) * maxSize, 0);

        // _CalcSeqCurrents(pElem, i012);
        if pElem^.NPhases = 3 then
        begin    // for 3-phase elements
            for j := 1 to pElem^.NTerms do
            begin
                k := (j - 1) * pElem^.NConds;
                Phase2SymComp(pComplexArray(@cBuffer[1 + k]), pComplexArray(i012));
                Inc(i012, 3);
            end;
            continue;
        end;

        // Handle non-3 phase elements
        if (pElem^.Nphases = 1) and posSeq then
        begin
            // Populate only phase 1 quantities in Pos seq
            i012 += 1;
            for j := 1 to pElem^.NTerms do
            begin
                k := (j - 1) * pElem^.NConds;
                i012^ := cBuffer[1 + k];
                Inc(i012, 3);  // inc to pos seq of next terminal
            end;
            Dec(i012);
        end
        // if neither 3-phase or pos seq model, just put in -1.0 for each element
        else
        begin
            for i := 1 to 3 * pElem^.NTerms do
            begin
                i012^ := -1;  // Signify n/A
                Inc(i012);
            end;
        end;
    end;
    
    if magnitude then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NTermsTotal * 3, 3, NTermsTotal);
        i012 := i012v;
        for i := 0 to NTermsTotal * 3 - 1 do
        begin
            Result[i] := Cabs(i012^);
            i012 += 1;
        end;
    end
    else
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NTermsTotal * 3 * 2, 3, NTermsTotal);
        Move(i012v^, ResultPtr[0], NTermsTotal * 3 * 2 * SizeOf(Double));
    end;

    ReallocMem(i012v, 0);
end;

procedure Alt_CEBatch_Get_SeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllxSeqCurrents(ResultPtr, ResultCount, batch, batchSize, True);
end;
procedure Alt_CEBatch_Get_ComplexSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllxSeqCurrents(ResultPtr, ResultCount, batch, batchSize, False);
end;
//------------------------------------------------------------------------------
procedure _Alt_CEBatch_Get_AllxSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; magnitude: boolean); CDECL;
// All voltages of all circuit element in the batch
// optionally, magnitude only
// returns a set of seq voltages (3) for each terminal
// 0, 1, 2 sequence  (0, +, -)
var
    outPtr: PDouble = NIL;
    idx, i, NTermsTotal, maxTerms: Integer;
    V012: pComplex;
    pElem: TDSSCktElementPtr;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or MissingSolution(TDSSCktElement(batch^)) then
    begin
        ResultCount[0] := 0;
        Exit;
    end;

    // Get sizes
    NTermsTotal := 0;
    maxTerms := 0;
    pElem := TDSSCktElementPtr(batch);
    for idx := 1 to batchSize do
    begin
        Inc(NTermsTotal, pElem^.NTerms);
        maxTerms := max(maxTerms, pElem^.Nterms);
        inc(pElem);
    end;

    if magnitude then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 3 * NTermsTotal, 3, NTermsTotal);
        outPtr := PDouble(ResultPtr);
        V012 := Allocmem(sizeof(Complex) * 3 * maxTerms);
        pElem := TDSSCktElementPtr(batch);
        for idx := 1 to batchSize do
        begin
            CalcSeqVoltages(pElem^, pComplexArray(V012));
            for i := 1 to 3 * pElem^.Nterms do
                outPtr[i - 1] := Cabs(V012[i - 1]);  // return mag only

            inc(outPtr, 3 * pElem^.NTerms);
            inc(pElem);
        end;
        Reallocmem(V012, 0);  // throw away temp memory
        Exit;
    end;

    // Results are complex numbers
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * 3 * NTermsTotal, 3, NTermsTotal);
    V012 := pComplex(ResultPtr);
    pElem := TDSSCktElementPtr(batch);
    for idx := 1 to batchSize do
    begin
        CalcSeqVoltages(pElem^, pComplexArray(V012));
        inc(V012, 3 * pElem^.NTerms);
        inc(pElem);
    end;
end;

procedure Alt_CEBatch_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllxSeqVoltages(ResultPtr, ResultCount, batch, batchSize, True);
end;
procedure Alt_CEBatch_Get_ComplexSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllxSeqVoltages(ResultPtr, ResultCount, batch, batchSize, False);
end;

//------------------------------------------------------------------------------
procedure _Alt_CEBatch_Get_AllCurrentsVoltages_x(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const What: Integer);
// What=1 for polar form, otherwise rectangular
type
    PPolar = ^Polar;
var
    pElem: TDSSCktElementPtr;
    cBuffer: pComplexArray;
    NValuesTotal, NValues, i, idx: Integer;
    CResultPtr: PComplex;
    CPolarResultPtr: PPolar;
    polarVal: Polar;
    NodeV: pNodeVArray;
begin
    if (batch = NIL) or (batch^ = NIL) or (batchSize = 0) or MissingSolution(TDSSCktElement(batch^)) then
    begin
        ResultCount[0] := 0;
        Exit;
    end;

    // Get the total number of (complex) elements
    NValuesTotal := 0;
    pElem := TDSSCktElementPtr(batch);
    for idx := 1 to batchSize do
    begin
        Inc(NValuesTotal, pElem^.NConds * pElem^.NTerms);
        inc(pElem);
    end;

    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NValuesTotal * 2);
    CPolarResultPtr := PPolar(ResultPtr);
    CResultPtr := PComplex(ResultPtr);

    // Get the actual values
    pElem := TDSSCktElementPtr(batch);
    if what < 2 then
    begin
        // Currents
        for idx := 1 to batchSize do
        begin
            NValues := pElem^.NConds * pElem^.NTerms;
            if pElem^.Enabled then
                pElem^.GetCurrents(pComplexArray(CResultPtr));
                
            Inc(CResultPtr, NValues);
            inc(pElem);
        end;
    end
    else
    begin
        // Voltages
        NodeV := pElem^.ActiveCircuit.Solution.NodeV;
        for idx := 1 to batchSize do
        begin
            NValues := pElem^.NConds * pElem^.NTerms;
            if pElem^.Enabled then
            begin
                for i := 1 to pElem^.NConds * pElem^.Nterms do
                begin
                    CResultPtr^ := NodeV[pElem^.NodeRef[i]]; // ok if =0
                    inc(CResultPtr);
                end;                
            end
            else
            begin
                Inc(CResultPtr, NValues);
            end;
            inc(pElem);
        end;    
    end;

    case What of
        1, 3: //Polar (Mag/Angle)
        begin
            cBuffer := pComplexArray(ResultPtr);
            if DSS_EXTENSIONS_ARRAY_DIMS then
            begin
                ResultCount[2] := 2;
                ResultCount[3] := NValuesTotal;
            end;

            for i := 1 to NValuesTotal do
            begin
                polarVal := ctopolardeg(cBuffer[i]);
                CPolarResultPtr^ := polarVal;
                inc(CPolarResultPtr);
            end;
        end;
    end;
end;

procedure Alt_CEBatch_Get_Currents(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllCurrentsVoltages_x(ResultPtr, ResultCount, batch, batchSize, 0);
end;
procedure Alt_CEBatch_Get_CurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllCurrentsVoltages_x(ResultPtr, ResultCount, batch, batchSize, 1);
end;
procedure Alt_CEBatch_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllCurrentsVoltages_x(ResultPtr, ResultCount, batch, batchSize, 2);
end;
procedure Alt_CEBatch_Get_VoltagesMagAng(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer); CDECL;
begin
    _Alt_CEBatch_Get_AllCurrentsVoltages_x(ResultPtr, ResultCount, batch, batchSize, 3);
end;

//------------------------------------------------------------------------------
function _Alt_PDElements_Get_pctCapacity_for(const AllNodes: Boolean; const What: integer; RatingIdx: Integer; pElem: TPDElement; cBuffer: pComplexArray): Double; inline;
// What=0 -> MaxCurrent
// What=1 -> pct of NormAmps
// What=2 -> pct of EmergAmps
var
    i: Integer;
    EmergAmps,
    NormAmps,
    Currmag,
    MaxCurrent: Double;
    NumNodes: Integer;
begin
    Result := 0;
    MaxCurrent := 0.0;
    
    if AllNodes then
        NumNodes := pElem.NConds * pElem.NTerms
    else
        NumNodes := pElem.Nphases;
    
    for i := 1 to NumNodes do
    begin
        Currmag := Cabs(cBuffer[i]);
        if Currmag > MaxCurrent then
            MaxCurrent := Currmag;
    end;
    if What = 0 then
    begin
        Result := MaxCurrent;
        Exit;
    end;
    
    NormAmps := pElem.NormAmps;
    EmergAmps := pElem.EmergAmps;
    if (RatingIdx <= pElem.NumAmpRatings) and (pElem.NumAmpRatings > 1) then
    begin
        NormAmps := pElem.AmpRatings[RatingIdx];
        EmergAmps := pElem.AmpRatings[RatingIdx];
    end;

    case What of
        1: if NormAmps <> 0 then Result := 100 * MaxCurrent / NormAmps;
        2: if EmergAmps <> 0 then Result := 100 * MaxCurrent / EmergAmps;
    end;
end;

function _Alt_PDE_Get_x(pElem: TPDElement; const What: integer; const AllNodes: Boolean; cresult: PComplex=NIL): Double;
// MaxCurrent (0), CapacityNorm (1), CapacityEmerg (2), Power (3)
var
    RatingIdx: Integer;
    // LocalPower: Complex;
    cBuffer: pComplexArray = NIL;
    RSignal: TXYCurveObj;
    DSS: TDSSContext;
begin
    Result := 0;
    if (not pElem.Enabled) or (pElem.NodeRef = NIL) or MissingSolution(pElem) then
        Exit;

    case What of // MaxCurrent (0), CapacityNorm (1), CapacityEmerg (2), Power (3)
    3: 
        begin
            cResult^ := pElem.Power[1] * 0.001;
        end;
    0, 1, 2:
        try
            RatingIdx := -1;
            DSS := pElem.DSS;
            if DSS.SeasonalRating then
            begin
                if DSS.SeasonSignal <> '' then
                begin
                    RSignal := DSS.XYCurveClass.Find(DSS.SeasonSignal);
                    if RSignal <> NIL then
                    begin
                        RatingIdx := trunc(RSignal.GetYValue(DSS.ActiveCircuit.Solution.DynaVars.intHour));
                    end
                    else
                        DSS.SeasonalRating := FALSE;   // The XYCurve defined doesn't exist
                end
                else
                    DSS.SeasonalRating := FALSE;    // The user didn't define the seasonal signal
            end;
            Getmem(cBuffer, sizeof(Complex) * pElem.Yorder);
            pElem.GetCurrents(cBuffer);
            Result := _Alt_PDElements_Get_pctCapacity_for(AllNodes, What, RatingIdx, pElem, cBuffer);
        except
            on E: Exception do
                DoSimpleMsg(DSS, 'Error processing currents: %s', [E.message], 5019);
        end;
    end;
    if Assigned(cBuffer) then
        Freemem(cBuffer);
end;

procedure _Alt_PDEBatch_Get_x(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const What: integer; const AllNodes: Boolean);
// Internal helper function to calculate for a batch of PDElements
// MaxCurrent (0), CapacityNorm (1), CapacityEmerg (2), Power (3)
type
    TPDElementPtr = ^TPDElement;
var
    Result: PDoubleArray0;
    k, idx, maxSize, RatingIdx: Integer;
    pElem: TPDElementPtr;
    LocalPower: Complex;
    cBuffer: pComplexArray = NIL;
    RSignal: TXYCurveObj;
    DSS: TDSSContext;
begin
    if batchSize = 0 then
    begin
        ResultCount[0] := 0;
        Exit;
    end;

    k := 0;
    pElem := TPDElementPtr(batch);
    DSS := pElem^.DSS;
    case What of  
    3: // Power (3)
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, batchSize * 2); // complex
            for idx := 1 to batchSize do
            begin
                if pElem^.Enabled then
                begin
                    LocalPower := pElem^.Power[1];
                    Result[k] := Localpower.re * 0.001;
                    Result[k + 1] := Localpower.im * 0.001;
                end;
                Inc(k, 2);
                inc(pElem);
            end;
        end;
    0, 1, 2: // MaxCurrent (0), CapacityNorm (1), CapacityEmerg (2),
        try
            RatingIdx := -1;
            if DSS.SeasonalRating then
            begin
                if DSS.SeasonSignal <> '' then
                begin
                    RSignal := DSS.XYCurveClass.Find(DSS.SeasonSignal);
                    if RSignal <> NIL then
                    begin
                        RatingIdx := trunc(RSignal.GetYValue(DSS.ActiveCircuit.Solution.DynaVars.intHour));
                    end
                    else
                        DSS.SeasonalRating := FALSE;   // The XYCurve defined doesn't exist
                end
                else
                    DSS.SeasonalRating := FALSE;    // The user didn't define the seasonal signal
            end;

            maxSize := 0;
            for idx := 1 to batchSize do
            begin
                maxSize := max(maxSize, pElem^.Yorder);
                inc(pElem);
            end;
            Getmem(cBuffer, sizeof(Complex) * maxSize);
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, batchSize); // real

            pElem := TPDElementPtr(batch);
            for idx := 1 to batchSize do
            begin
                if pElem^.Enabled then
                begin
                    pElem^.GetCurrents(cBuffer);
                    Result[k] := _Alt_PDElements_Get_pctCapacity_for(AllNodes, What, RatingIdx, pElem^, cBuffer);
                end;
                Inc(k);
                inc(pElem);
            end;
        except
            on E: Exception do
                DoSimpleMsg(DSS, 'Error processing currents: %s', [E.message], 5019);
        end;
    end;
    if Assigned(cBuffer) then
        Freemem(cBuffer);
end;

function Alt_PDE_Get_pctNorm(elem: TPDElement; const AllNodes: TAltAPIBoolean): Double; CDECL;
begin
    Result := _Alt_PDE_Get_x(elem, 1, AllNodes);
end;

function Alt_PDE_Get_pctEmerg(elem: TPDElement; const AllNodes: TAltAPIBoolean): Double; CDECL;
begin
    Result := _Alt_PDE_Get_x(elem, 2, AllNodes);
end;

procedure Alt_PDEBatch_Get_pctNorm(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const AllNodes: TAltAPIBoolean); CDECL;
begin
    _Alt_PDEBatch_Get_x(ResultPtr, ResultCount, batch, batchSize, 1, AllNodes);
end;

procedure Alt_PDEBatch_Get_pctEmerg(var ResultPtr: PDouble; ResultCount: PAPISize; batch: TDSSCktElementPtr; batchSize: Integer; const AllNodes: TAltAPIBoolean); CDECL;
begin
    _Alt_PDEBatch_Get_x(ResultPtr, ResultCount, batch, batchSize, 2, AllNodes);
end;

end.
