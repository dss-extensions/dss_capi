unit CAPI_Bus;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function Bus_Get_Name(): PAnsiChar; CDECL;
function Bus_Get_NumNodes(): Integer; CDECL;
procedure Bus_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_SeqVoltages_GR(); CDECL;
procedure Bus_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_Voltages_GR(); CDECL;
procedure Bus_Get_Nodes(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Bus_Get_Nodes_GR(); CDECL;
procedure Bus_Get_Isc(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_Isc_GR(); CDECL;
procedure Bus_Get_Voc(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_Voc_GR(); CDECL;
function Bus_Get_kVBase(): Double; CDECL;
procedure Bus_Get_puVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_puVoltages_GR(); CDECL;
procedure Bus_Get_Zsc0(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_Zsc0_GR(); CDECL;
procedure Bus_Get_Zsc1(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_Zsc1_GR(); CDECL;
procedure Bus_Get_ZscMatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_ZscMatrix_GR(); CDECL;
function Bus_ZscRefresh(): TAPIBoolean; CDECL;
procedure Bus_Get_YscMatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_YscMatrix_GR(); CDECL;
function Bus_Get_Coorddefined(): TAPIBoolean; CDECL;
function Bus_Get_x(): Double; CDECL;
procedure Bus_Set_x(Value: Double); CDECL;
function Bus_Get_y(): Double; CDECL;
procedure Bus_Set_y(Value: Double); CDECL;
function Bus_Get_Distance(): Double; CDECL;
function Bus_GetUniqueNodeNumber(StartNumber: Integer): Integer; CDECL;
procedure Bus_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_CplxSeqVoltages_GR(); CDECL;
function Bus_Get_Int_Duration(): Double; CDECL;
function Bus_Get_Lambda(): Double; CDECL;
function Bus_Get_Cust_Duration(): Double; CDECL;
function Bus_Get_Cust_Interrupts(): Double; CDECL;
function Bus_Get_N_Customers(): Integer; CDECL;
function Bus_Get_N_interrupts(): Double; CDECL;
procedure Bus_Get_puVLL(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_puVLL_GR(); CDECL;
procedure Bus_Get_VLL(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_VLL_GR(); CDECL;
procedure Bus_Get_puVmagAngle(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_puVmagAngle_GR(); CDECL;
procedure Bus_Get_VMagAngle(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_VMagAngle_GR(); CDECL;
function Bus_Get_TotalMiles(): Double; CDECL;
function Bus_Get_SectionID(): Integer; CDECL;
function Bus_Get_Next(): Integer; CDECL; // API Extension
procedure Bus_Get_LineList(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Bus_Get_LineList_GR(); CDECL;
procedure Bus_Get_LoadList(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Bus_Get_LoadList_GR(); CDECL;
procedure Bus_Get_ZSC012Matrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Bus_Get_ZSC012Matrix_GR(); CDECL;
procedure Bus_Get_AllPCEatBus(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Bus_Get_AllPDEatBus(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Circuit,
    UComplex, DSSUcomplex,
    MathUtil,
    sysutils,
    ExecHelper,
    SolutionAlgs,
    Utilities,
    Bus,
    CktElement,
    Solution,
    Ucmatrix,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _hasActiveBus(DSS: TDSSContext): Boolean; inline;
begin
    Result := False;
    if InvalidCircuit(DSS) then
        Exit;

    if (not ((DSS.ActiveCircuit.ActiveBusIndex > 0) and (DSS.ActiveCircuit.ActiveBusIndex <= DSS.ActiveCircuit.NumBuses))) or
       (DSS.ActiveCircuit.Buses = NIL) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, _('No active bus found! Activate one and retry.'), 8989);
        end;
        Exit;
    end;
    Result := True;
end;
//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TDSSBus): Boolean; inline;
begin
    Result := False;
    obj := NIL;

    if not _hasActiveBus(DSS) then
        Exit;

    obj := DSS.ActiveCircuit.Buses[DSS.ActiveCircuit.ActiveBusIndex];
    Result := True;
end;
//------------------------------------------------------------------------------
function Bus_Get_Name(): PAnsiChar; CDECL;
var
    pBus: TDSSBus;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, pBus.Name);
end;
//------------------------------------------------------------------------------
function Bus_Get_NumNodes(): Integer; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    
    Result := pBus.NumNodesThisBus;
end;
//------------------------------------------------------------------------------
procedure Bus_Get_SeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Compute sequence voltages for Active Bus
// magnitude only
// returns a set of seq voltages (3)
var
    Result: PDoubleArray0;
    Nvalues, i, iV: Integer;
    VPh, V012: Complex3;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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
        Vph[i] := DSSPrime.ActiveCircuit.Solution.NodeV[pBus.Find(i)];
    end;

    Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

    for i := 1 to 3 do  // Stuff it in the result
    begin
        Result[iV] := Cabs(V012[i]);
        Inc(iV);
    end;
end;

procedure Bus_Get_SeqVoltages_GR(); CDECL;
// Same as Bus_Get_SeqVoltages but uses global result (GR) pointers
begin
    Bus_Get_SeqVoltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_Voltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return Complex for all nodes of voltages for Active Bus
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: Complex;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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

        Volts := DSSPrime.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)];
            Result[iV] := Volts.re;
            Inc(iV);
            Result[iV] := Volts.im;
            Inc(iV);
        end;
end;

procedure Bus_Get_Voltages_GR(); CDECL;
// Same as Bus_Get_Voltages but uses global result (GR) pointers
begin
    Bus_Get_Voltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_Nodes(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
// return array of node numbers corresponding to voltages
var
    Result: PIntegerArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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

procedure Bus_Get_Nodes_GR(); CDECL;
// Same as Bus_Get_Nodes but uses global result (GR) pointers
begin
    Bus_Get_Nodes(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_Isc(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return the short circuit current
var
    Result: PDoubleArray0;
    Isc: Complex;
    i, iV, NValues: Integer;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

        if pBus.BusCurrent <> NIL then
        begin
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
        end
        else
            DefaultResult(ResultPtr, ResultCount);
end;

procedure Bus_Get_Isc_GR(); CDECL;
// Same as Bus_Get_Isc but uses global result (GR) pointers
begin
    Bus_Get_Isc(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_Voc(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return the Open circuit Voltage for this bus
var
    Result: PDoubleArray0;
    Voc: Complex;
    i, iV, NValues: Integer;

    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    if pBus.VBus <> NIL then
    begin
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
        end
        else
            DefaultResult(ResultPtr, ResultCount);
end;

procedure Bus_Get_Voc_GR(); CDECL;
// Same as Bus_Get_Voc but uses global result (GR) pointers
begin
    Bus_Get_Voc(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Bus_Get_kVBase(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := pBus.kVBase;
end;
//------------------------------------------------------------------------------
procedure Bus_Get_puVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Returns voltages at bus in per unit.  However, if kVBase=0, returns actual volts
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: Complex;
    BaseFactor: Double;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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

        Volts := DSSPrime.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)];
            Result[iV] := Volts.re / BaseFactor;
            Inc(iV);
            Result[iV] := Volts.im / BaseFactor;
            Inc(iV);
        end;
    end;

procedure Bus_Get_puVoltages_GR(); CDECL;
// Same as Bus_Get_puVoltages but uses global result (GR) pointers
begin
    Bus_Get_puVoltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_Zsc0(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Z: Complex;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

        Z := pBus.Zsc0;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z.Re;
        Result[1] := Z.Im;
end;

procedure Bus_Get_Zsc0_GR(); CDECL;
// Same as Bus_Get_Zsc0 but uses global result (GR) pointers
begin
    Bus_Get_Zsc0(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_Zsc1(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Z: Complex;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

        Z := pBus.Zsc1;
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
        Result[0] := Z.Re;
        Result[1] := Z.Im;
end;

procedure Bus_Get_Zsc1_GR(); CDECL;
// Same as Bus_Get_Zsc1 but uses global result (GR) pointers
begin
    Bus_Get_Zsc1(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_ZscMatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Nelements, iV, i, j: Integer;
    Z: Complex;
    pBus: TDSSBus;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    try
            if Assigned(pBus.Zsc) then
            begin
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
        end;
    except
        On E: Exception do
            DoSimpleMsg(DSSPrime, 'ZscMatrix Error: %s', [E.message], 5016);
    end;
end;

procedure Bus_Get_ZscMatrix_GR(); CDECL;
// Same as Bus_Get_ZscMatrix but uses global result (GR) pointers
begin
    Bus_Get_ZscMatrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Bus_ZscRefresh(): TAPIBoolean; CDECL;
begin
    Result := FALSE;   // Init in case of failure

    if DSSPrime.DSSExecutive.DoZscRefresh = 0 then
        Result := TRUE;

end;
//------------------------------------------------------------------------------
procedure Bus_Get_YscMatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Nelements, iV, i, j: Integer;
    Y1: Complex;
    pBus: TDSSBus;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    try
            if Assigned(pBus.Ysc) then
            begin
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
            end
    except
        On E: Exception do
            DoSimpleMsg(DSSPrime, 'ZscMatrix Error: %s', [E.message], 5017);
    end;
end;

procedure Bus_Get_YscMatrix_GR(); CDECL;
// Same as Bus_Get_YscMatrix but uses global result (GR) pointers
begin
    Bus_Get_YscMatrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Bus_Get_Coorddefined(): TAPIBoolean; CDECL;
var
    pBus: TDSSBus;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    if pBus.CoordDefined then
                Result := TRUE;
end;
//------------------------------------------------------------------------------
function Bus_Get_x(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    if pBus.CoordDefined then
        Result := pBus.x;
end;
//------------------------------------------------------------------------------
procedure Bus_Set_x(Value: Double); CDECL;
var
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    pBus.CoordDefined := TRUE;
    pBus.x := Value;
end;
//------------------------------------------------------------------------------
function Bus_Get_y(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    if pBus.CoordDefined then
        Result := pBus.y;
end;
//------------------------------------------------------------------------------
procedure Bus_Set_y(Value: Double); CDECL;
var
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    pBus.CoordDefined := TRUE;
    pBus.y := Value;
end;
//------------------------------------------------------------------------------
function Bus_Get_Distance(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    Result := pBus.DistFromMeter;
end;
//------------------------------------------------------------------------------
function Bus_GetUniqueNodeNumber(StartNumber: Integer): Integer; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := DSSPrime.ActiveCircuit.GetUniqueNodeNumber(pBus.Name, StartNumber);
end;
//------------------------------------------------------------------------------
procedure Bus_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Compute sequence voltages for Active Bus
// Complex values
// returns a set of seq voltages (3) in 0, 1, 2 order
var
    Result: PDoubleArray0;
    Nvalues, i, iV: Integer;
    VPh, V012: Complex3;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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
            Vph[i] := DSSPrime.ActiveCircuit.Solution.NodeV[pBus.Find(i)];

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

procedure Bus_Get_CplxSeqVoltages_GR(); CDECL;
// Same as Bus_Get_CplxSeqVoltages but uses global result (GR) pointers
begin
    Bus_Get_CplxSeqVoltages(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Bus_Get_Int_Duration(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := pBus.Bus_Int_Duration;
end;
//------------------------------------------------------------------------------
function Bus_Get_Lambda(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := pBus.BusFltRate;
end;
//------------------------------------------------------------------------------
function Bus_Get_Cust_Duration(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := pBus.BusCustDurations;
end;
//------------------------------------------------------------------------------
function Bus_Get_Cust_Interrupts(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := pBus.BusCustInterrupts;
end;
//------------------------------------------------------------------------------
function Bus_Get_N_Customers(): Integer; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := pBus.BusTotalNumCustomers;
end;
//------------------------------------------------------------------------------
function Bus_Get_N_interrupts(): Double; CDECL;
var
    pBus: TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;

    Result := pBus.Bus_Num_Interrupt;
end;
//------------------------------------------------------------------------------
procedure Bus_Get_puVLL(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdxi, NodeIdxj, jj, k: Integer;
    Volts: Complex;
    pBus: TDSSBus;
    BaseFactor: Double;
    NodeV: pNodeVArray;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    NodeV := DSSPrime.ActiveCircuit.Solution.NodeV;
        Nvalues := pBus.NumNodesThisBus;
        if Nvalues > 3 then
            Nvalues := 3;

        if Nvalues > 1 then
        begin
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

        end
        else
        begin  // for 1-phase buses, do not attempt to compute.
            //TODO: actual error?
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
            Result[0] := -99999.0;
            Result[1] := 0.0;
        end;
end;

procedure Bus_Get_puVLL_GR(); CDECL;
// Same as Bus_Get_puVLL but uses global result (GR) pointers
begin
    Bus_Get_puVLL(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_VLL(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdxi, NodeIdxj, jj, k: Integer;
    Volts: Complex;
    pBus: TDSSBus;
    NodeV: pNodeVArray;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    NodeV := DSSPrime.ActiveCircuit.Solution.NodeV;
        Nvalues := pBus.NumNodesThisBus;
        if Nvalues > 3 then
            Nvalues := 3;

        if Nvalues > 1 then
        begin
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
        end
        else
        begin  // for 1-phase buses, do not attempt to compute.
            //TODO: actual error?
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2);
            Result[0] := -99999.0;
            Result[1] := 0.0;
        end;
end;

procedure Bus_Get_VLL_GR(); CDECL;
// Same as Bus_Get_VLL but uses global result (GR) pointers
begin
    Bus_Get_VLL(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_puVmagAngle(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return mag/angle for all nodes of voltages for Active Bus
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: polar;
    pBus: TDSSBus;
    Basefactor: Double;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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

        Volts := ctopolardeg(DSSPrime.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)]);
            Result[iV] := Volts.mag / BaseFactor;
            Inc(iV);
            Result[iV] := Volts.ang;
            Inc(iV);
        end;
end;

procedure Bus_Get_puVmagAngle_GR(); CDECL;
// Same as Bus_Get_puVmagAngle but uses global result (GR) pointers
begin
    Bus_Get_puVmagAngle(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_VMagAngle(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return mag/angle for all nodes of voltages for Active Bus
var
    Result: PDoubleArray0;
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: polar;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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

        Volts := ctopolardeg(DSSPrime.ActiveCircuit.Solution.NodeV[pBus.GetRef(NodeIdx)]);
            Result[iV] := Volts.mag;
            Inc(iV);
            Result[iV] := Volts.ang;
            Inc(iV);
        end;
end;

procedure Bus_Get_VMagAngle_GR(); CDECL;
// Same as Bus_Get_VMagAngle but uses global result (GR) pointers
begin
    Bus_Get_VMagAngle(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Bus_Get_TotalMiles(): Double; CDECL;
var
    pBus : TDSSBus;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    Result := pBus.BusTotalMiles;
end;
//------------------------------------------------------------------------------
function Bus_Get_SectionID(): Integer; CDECL;
var
    pBus : TDSSBus;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    Result := pBus.BusSectionID;
end;
//------------------------------------------------------------------------------
function Bus_Get_Next(): Integer; CDECL;
var
    BusIndex: Integer;
begin
    Result := -1;   // Signifies Error
    if InvalidCircuit(DSSPrime) then
        Exit;
    BusIndex := DSSPrime.ActiveCircuit.ActiveBusIndex + 1;
    if (BusIndex > 0) and (BusIndex <= DSSPrime.ActiveCircuit.Numbuses) then
    begin
        DSSPrime.ActiveCircuit.ActiveBusIndex := BusIndex;
            Result := 0;
        end;
    end;
//------------------------------------------------------------------------------
function CheckBusReference(cktElem: TDSSCktElement; BusReference: Integer; var TerminalIndex: Integer): Boolean;
// Check all terminals of cktelement to see if bus connected to busreference
var
    i: Integer;
begin
    Result := FALSE;
    for i := 1 to cktElem.NTerms do
        begin
        if cktElem.Terminals[i - 1].BusRef = BusReference then
            begin
                TerminalIndex := i;
                Result := TRUE;
                Break;
            end;
        end;
end;
//------------------------------------------------------------------------------
procedure Bus_Get_LineList(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
// Returns list of LINE elements connected to this bus
var
    BusReference, j, k, LineCount: Integer;
    pElem: TDSSCktElement;
    Result: PPAnsiCharArray0;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;

    BusReference := DSSPrime.ActiveCircuit.ActiveBusIndex;
    // Count number of Lines connected to this bus
        LineCount := 0;
    for pElem in DSSPrime.ActiveCircuit.Lines do
        begin
            if CheckBusReference(pElem, BusReference, j) then
                Inc(LineCount);
        end;

        if LineCount <= 0 then
        begin
            DefaultResult(ResultPtr, ResultCount, '');
            Exit;
        end;

        //TODO: save list of elements to avoid a second loop through them all?

        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, LineCount);
        k := 0;
    for pElem in DSSPrime.ActiveCircuit.Lines do
        begin
            if CheckBusReference(pElem, BusReference, j) then
            begin
                Result[k] := DSS_CopyStringAsPChar('LINE.' + pElem.name);
                Inc(k);
            end;
    end;
end;

procedure Bus_Get_LineList_GR(); CDECL;
// Same as Bus_Get_LineList but uses global result (GR) pointers
begin
    Bus_Get_LineList(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_LoadList(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
// Returns list of LOAD elements connected to this bus
var
    BusReference, j, k, LoadCount: Integer;
    pElem: TDSSCktElement;
    Result: PPAnsiCharArray0;
    pBus : TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount, '');
        Exit;
    end;

    BusReference := DSSPrime.ActiveCircuit.ActiveBusIndex;
        // Count number of LOAD elements connected to this bus
        LoadCount := 0;
    for pElem in DSSPrime.ActiveCircuit.Loads do
        begin
            if CheckBusReference(pElem, BusReference, j) then
                Inc(LoadCount);
        end;

        if LoadCount <= 0 then
        begin
            DefaultResult(ResultPtr, ResultCount, '');
            Exit;
        end;

        //TODO: save list of elements to avoid a second loop through them all?

        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, LoadCount);
        k := 0;
    for pElem in DSSPrime.ActiveCircuit.Loads do
        begin
            if CheckBusReference(pElem, BusReference, j) then
            begin
                Result[k] := DSS_CopyStringAsPChar('LOAD.' + pElem.name);
                Inc(k);
            end;
        end;
    end;

procedure Bus_Get_LoadList_GR(); CDECL;
// Same as Bus_Get_LoadList but uses global result (GR) pointers
begin
    Bus_Get_LoadList(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_ZSC012Matrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL; //TODO: remove duplication between this and DoZsc012Cmd
var
    Zsc012Temp: TCmatrix;
    NValues: Integer;
    Norder: Integer;
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

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

procedure Bus_Get_ZSC012Matrix_GR(); CDECL;
// Same as Bus_Get_ZSC012Matrix but uses global result (GR) pointers
begin
    Bus_Get_ZSC012Matrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_AllPCEatBus(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    i: Integer;
    pces: Array of String;
    Result: PPAnsiCharArray0;
begin
    if not _hasActiveBus(DSSPrime) then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    pces := DSSPrime.ActiveCircuit.getPCEatBus(DSSPrime.ActiveCircuit.ActiveBusIndex, False);
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(pces));
    for i := 0 to High(pces) do
        Result[i] := DSS_CopyStringAsPChar(pces[i]);
end;
//------------------------------------------------------------------------------
procedure Bus_Get_AllPDEatBus(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
  i: Integer;
  pdes: Array of String;
  Result: PPAnsiCharArray0;
begin
    if not _hasActiveBus(DSSPrime) then
    begin
        DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Exit;
    end;
    pdes := DSSPrime.ActiveCircuit.getPDEatBus(DSSPrime.ActiveCircuit.ActiveBusIndex, False);
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(pdes));
    for i := 0 to High(pdes) do
        Result[i] := DSS_CopyStringAsPChar(pdes[i]);
end;
//------------------------------------------------------------------------------
end.
