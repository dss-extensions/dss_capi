unit CAPI_PDElements;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function PDElements_Get_Count(): Integer; CDECL;
function PDElements_Get_FaultRate(): Double; CDECL;
function PDElements_Get_First(): Integer; CDECL;
function PDElements_Get_IsShunt(): TAPIBoolean; CDECL;
function PDElements_Get_Next(): Integer; CDECL;
function PDElements_Get_pctPermanent(): Double; CDECL;
procedure PDElements_Set_FaultRate(Value: Double); CDECL;
procedure PDElements_Set_pctPermanent(Value: Double); CDECL;
function PDElements_Get_Name(): PAnsiChar; CDECL;
procedure PDElements_Set_Name(const Value: PAnsiChar); CDECL;
function PDElements_Get_AccumulatedL(): Double; CDECL;
function PDElements_Get_Lambda(): Double; CDECL;
function PDElements_Get_Numcustomers(): Integer; CDECL;
function PDElements_Get_ParentPDElement(): Integer; CDECL;
function PDElements_Get_RepairTime(): Double; CDECL;
function PDElements_Get_Totalcustomers(): Integer; CDECL;
function PDElements_Get_FromTerminal(): Integer; CDECL;
function PDElements_Get_TotalMiles(): Double; CDECL;
function PDElements_Get_SectionID(): Integer; CDECL;
procedure PDElements_Set_RepairTime(Value: Double); CDECL;

// Extensions below
procedure PDElements_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllNames_GR(); CDECL;
procedure PDElements_Get_AllMaxCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; const AllNodes: TAPIBoolean); CDECL;
procedure PDElements_Get_AllMaxCurrents_GR(const AllNodes: TAPIBoolean); CDECL;
procedure PDElements_Get_AllPctNorm(var ResultPtr: PDouble; ResultCount: PAPISize; const AllNodes: TAPIBoolean); CDECL;
procedure PDElements_Get_AllPctNorm_GR(const AllNodes: TAPIBoolean); CDECL;
procedure PDElements_Get_AllPctEmerg(var ResultPtr: PDouble; ResultCount: PAPISize; const AllNodes: TAPIBoolean); CDECL;
procedure PDElements_Get_AllPctEmerg_GR(const AllNodes: TAPIBoolean); CDECL;
procedure PDElements_Get_AllCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllCurrents_GR(); CDECL;
procedure PDElements_Get_AllCurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllCurrentsMagAng_GR(); CDECL;
procedure PDElements_Get_AllCplxSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllCplxSeqCurrents_GR(); CDECL;
procedure PDElements_Get_AllSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllSeqCurrents_GR(); CDECL;
procedure PDElements_Get_AllPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllPowers_GR(); CDECL;
procedure PDElements_Get_AllSeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllSeqPowers_GR(); CDECL;
procedure PDElements_Get_AllNumPhases(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllNumPhases_GR(); CDECL;
procedure PDElements_Get_AllNumConductors(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllNumConductors_GR(); CDECL;
procedure PDElements_Get_AllNumTerminals(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure PDElements_Get_AllNumTerminals_GR(); CDECL;

implementation

uses
    CAPI_Alt,
    CAPI_Constants,
    DSSGlobals,
    PDElement,
    PDClass,
    SysUtils,
    DSSPointerList,
    Bus,
    Solution,
    XYCurve,
    UComplex, DSSUcomplex,
    ArrayDef,
    Utilities,
    Math,
    MathUtil,
    DSSClass,
    DSSHelper,
    DSSObject;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TPDElement): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    if DSS.ActiveCircuit.ActiveCktElement = NIL then 
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, _('No active PD Element found! Activate one and retry.'), 8989);
        end;
        Exit;
    end;

    if not (DSS.ActiveCircuit.ActiveCktElement is TPDElement) then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, _('No active PD Element found! Activate one and retry.'), 8989);
        end;
        Exit;
    end;
        
    obj := DSS.ActiveCircuit.ActiveCktElement as TPDElement;
    Result := True;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    
    Result := DSSPrime.ActiveCircuit.PDElements.Count;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FaultRate(): Double; CDECL;
var
    elem: TPDElement;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Faultrate;
end;
//------------------------------------------------------------------------------
function PDElements_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.PDElements);
end;
//------------------------------------------------------------------------------
function PDElements_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.PDelements);
    if Result <> 0 then Result := 1; //TODO: inconsistent with the rest
end;
//------------------------------------------------------------------------------
function PDElements_Get_IsShunt(): TAPIBoolean; CDECL;
var
    elem: TPDElement;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.IsShunt;
end;
//------------------------------------------------------------------------------
function PDElements_Get_pctPermanent(): Double; CDECL;
var
    elem: TPDElement;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PctPerm;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_FaultRate(Value: Double); CDECL;
var
    elem: TPDElement;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FaultRate := Value;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_pctPermanent(Value: Double); CDECL;
var
    elem: TPDElement;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.PctPerm := Value;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Name(): PAnsiChar; CDECL;
var
    elem: TPDElement;
begin
    Result := NIL;   // return null if not a PD element
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.FullName);  // full name
end;

//------------------------------------------------------------------------------
procedure PDElements_Set_Name(const Value: PAnsiChar); CDECL; //TODO: rewrite to use a hashmap?
var
    elem: TPDElement;
    TestString: String;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    TestString := Value;
    // Search through list of PD Elements until we find this one
    for elem in DSSPrime.ActiveCircuit.PDElements do
    begin
        if (AnsiCompareText(TestString, elem.FullName) = 0) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := elem;
            break;
        end;
    end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_AccumulatedL(): Double; CDECL;
var
    elem: TPDElement;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.AccumulatedBrFltRate;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Lambda(): Double; CDECL;
var
    elem: TPDElement;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.BranchFltRate;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Numcustomers(): Integer; CDECL;
var
    elem: TPDElement;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.BranchNumCustomers;
end;
//------------------------------------------------------------------------------
function PDElements_Get_ParentPDElement(): Integer; CDECL;
var
    elem: TPDElement;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.ParentPDElement <> NIL then    // leaves ActiveCktElement as is
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := elem.ParentPDElement;
        Result := DSSPrime.ActiveCircuit.ActivecktElement.ClassIndex;
    end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_RepairTime(): Double; CDECL;
var
    elem: TPDElement;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.HrsToRepair;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Totalcustomers(): Integer; CDECL;
var
    elem: TPDElement;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    if DSSPrime.ActiveCircuit.ActiveCktElement is TPDElement then
    begin
        elem := DSSPrime.ActiveCircuit.ActiveCktelement as TPDElement;
        Result := elem.BranchTotalCustomers;
    end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FromTerminal(): Integer; CDECL;
var
    elem: TPDElement;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FromTerminal;
end;
//------------------------------------------------------------------------------
function PDElements_Get_TotalMiles(): Double; CDECL;
// Total miles of line from here on down to the end of the feeder
var
    elem: TPDElement;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.AccumulatedMilesDownStream;
end;
//------------------------------------------------------------------------------
function PDElements_Get_SectionID(): Integer; CDECL;
var
    elem: TPDElement;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.BranchSectionID;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_RepairTime(Value: Double); CDECL;
var
    elem: TPDElement;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.HrsToRepair := Value;
end;
//------------------------------------------------------------------------------
procedure PDElements_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    idx_before, k, numEnabled: Integer;
    elem: TPDElement;
    pList: TDSSPointerList;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;

    pList := DSSPrime.ActiveCircuit.PDElements;
    if pList.Count <= 0 then
        Exit;
    
    idx_before := pList.ActiveIndex;
    k := 0;
    numEnabled := pList.Count;
//    for elem in pList do
//    begin
//        if elem.Enabled then 
//            Inc(numEnabled);
//    end;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, numEnabled);

    for elem in pList do
    begin
        // if (elem.Enabled or DSS_CAPI_ITERATE_DISABLED) then
        begin
            Result[k] := DSS_CopyStringAsPChar(elem.FullName);
            Inc(k);
        end;
    end;
    if (idx_before > 0) and (idx_before <= pList.Count) then
        pList.Get(idx_before);
end;

procedure PDElements_Get_AllNames_GR(); CDECL;
// Same as PDElements_Get_AllNames but uses global result (GR) pointers
begin
    PDElements_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0]);
end;

//------------------------------------------------------------------------------
procedure _PDElements_Get_x(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; const What: integer; const AllNodes: Boolean);
// Internal helper function to calculate for all PDElements
// MaxCurrent (0), CapacityNorm (1), CapacityEmerg (2), Power (3)
var
    pList: TDSSPointerList;
begin
    if (MissingSolution(DSS)) or (DSS.ActiveCircuit.PDElements.Count <= 0) then 
    begin
        DefaultResult(ResultPtr, ResultCount, -1.0);
        Exit;
    end;
    pList := DSS.ActiveCircuit.PDElements;
    _Alt_PDEBatch_Get_x(ResultPtr, ResultCount, TDSSCktElementPtr(pList.InternalPointer), pList.Count, What, AllNodes);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllMaxCurrents(var ResultPtr: PDouble; ResultCount: PAPISize; const AllNodes: TAPIBoolean); CDECL;
begin
    _PDElements_Get_x(DSSPrime, ResultPtr, ResultCount, 0, AllNodes);
end;

procedure PDElements_Get_AllMaxCurrents_GR(const AllNodes: TAPIBoolean); CDECL;
// Same as PDElements_Get_AllMaxCurrents but uses global result (GR) pointers
begin
    PDElements_Get_AllMaxCurrents(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, AllNodes)
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllPctNorm(var ResultPtr: PDouble; ResultCount: PAPISize; const AllNodes: TAPIBoolean); CDECL;
begin
    _PDElements_Get_x(DSSPrime, ResultPtr, ResultCount, 1, AllNodes);
end;

procedure PDElements_Get_AllPctNorm_GR(const AllNodes: TAPIBoolean); CDECL;
// Same as PDElements_Get_AllPctNorm but uses global result (GR) pointers
begin
    PDElements_Get_AllPctNorm(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, AllNodes)
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllPctEmerg(var ResultPtr: PDouble; ResultCount: PAPISize; const AllNodes: TAPIBoolean); CDECL;
begin
    _PDElements_Get_x(DSSPrime, ResultPtr, ResultCount, 2, AllNodes);
end;

procedure PDElements_Get_AllPctEmerg_GR(const AllNodes: TAPIBoolean); CDECL;
// Same as PDElements_Get_AllPctEmerg but uses global result (GR) pointers
begin
    PDElements_Get_AllPctEmerg(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, AllNodes)
end;

//------------------------------------------------------------------------------
procedure _PDElements_Get_AllCurrents_x(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; const What: Integer);
// What=1 for polar form, otherwise rectangular
var
    pList: TDSSPointerList;
begin
    if (InvalidCircuit(DSS)) or (DSS.ActiveCircuit.PDElements.Count <= 0) then 
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    pList := DSS.ActiveCircuit.PDElements;
    _Alt_CEBatch_Get_AllCurrentsVoltages_x(ResultPtr, ResultCount, TDSSCktElementPtr(pList.InternalPointer), pList.Count, What);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
begin
    _PDElements_Get_AllCurrents_x(DSSPrime, ResultPtr, ResultCount, 0);
end;

procedure PDElements_Get_AllCurrents_GR(); CDECL;
// Same as PDElements_Get_AllCurrentsMagAng but uses global result (GR) pointers
begin
    PDElements_Get_AllCurrents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0]);
end;
//------------------------------------------------------------------------------
procedure PDElements_Get_AllCurrentsMagAng(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
begin
    _PDElements_Get_AllCurrents_x(DSSPrime, ResultPtr, ResultCount, 1);
end;

procedure PDElements_Get_AllCurrentsMagAng_GR(); CDECL;
// Same as PDElements_Get_AllCurrentsMagAng but uses global result (GR) pointers
begin
    PDElements_Get_AllCurrentsMagAng(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0]);
end;

//------------------------------------------------------------------------------
procedure _PDElements_Get_AllxSeqCurrents(DSS: TDSSContext; var ResultPtr: PDouble; ResultCount: PAPISize; magnitude: boolean);
var
    pList: TDSSPointerList;
begin
    if (MissingSolution(DSS)) or (DSS.ActiveCircuit.PDElements.Count <= 0) then 
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    pList := DSS.ActiveCircuit.PDElements;
    _Alt_CEBatch_Get_AllxSeqCurrents(ResultPtr, ResultCount, TDSSCktElementPtr(pList.InternalPointer), pList.Count, magnitude);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
begin
    _PDElements_Get_AllxSeqCurrents(DSSPrime, ResultPtr, ResultCount, True);
end;

procedure PDElements_Get_AllSeqCurrents_GR(); CDECL;
// Same as PDElements_Get_AllSeqCurrents but uses global result (GR) pointers
begin
    PDElements_Get_AllSeqCurrents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0]);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllCplxSeqCurrents(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
begin
    _PDElements_Get_AllxSeqCurrents(DSSPrime, ResultPtr, ResultCount, False);
end;

procedure PDElements_Get_AllCplxSeqCurrents_GR(); CDECL;
// Same as PDElements_Get_AllCplxSeqCurrents but uses global result (GR) pointers
begin
    PDElements_Get_AllCplxSeqCurrents(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0]);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Return complex kW, kvar in each conductor for each terminal, for each PDElement
var
    pList: TDSSPointerList;
begin
    if (InvalidCircuit(DSSPrime)) or (DSSPrime.ActiveCircuit.PDElements.Count <= 0) then 
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    pList := DSSPrime.ActiveCircuit.PDElements;
    Alt_CEBatch_Get_Powers(ResultPtr, ResultCount, TDSSCktElementPtr(pList.InternalPointer), pList.Count);
end;

procedure PDElements_Get_AllPowers_GR(); CDECL;
// Same as PDElements_Get_AllPowers but uses global result (GR) pointers
begin
    PDElements_Get_AllPowers(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0]);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllSeqPowers(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// All seq Powers of each PD element
// returns kW + j kvar
var
    Result: PDoubleArray0;
    NValuesTotal, MaxNValues, i, j, k, n, iCount: Integer;
    idx_before: Integer;
    pList: TDSSPointerList;
    pElem: TPDElement;
    CResultPtr: PComplex;
    cBuffer: pComplexArray;
    VPh, V012: Complex3;
    IPh, I012: Complex3;
    S: Complex;
    NodeV: pNodeVarray;
begin
    if (MissingSolution(DSSPrime)) or (DSSPrime.ActiveCircuit.PDElements.Count <= 0) then 
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    NodeV := DSSPrime.ActiveCircuit.Solution.NodeV;
    pList := DSSPrime.ActiveCircuit.PDElements;
    idx_before := pList.ActiveIndex;

    // Get the total number of (complex) elements
    NValuesTotal := 0;
    MaxNValues := 0;
    
    for pElem in pList do
    begin
        if pElem.Enabled then
        begin
            Inc(NValuesTotal, 3 * pElem.NTerms);
            MaxNValues := Max(MaxNValues, pElem.NConds * pElem.NTerms);
        end;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NValuesTotal * 2);
    CResultPtr := PComplex(ResultPtr);
    cBuffer := Allocmem(sizeof(Complex) * MaxNValues);

    // Get the actual values
    iCount := 0;
    for pElem in pList do
    begin
//            if not pElem.Enabled then  
//            begin
//                continue;
//            end;
        if pElem.NPhases <> 3 then
        begin
            if (pElem.Nphases = 1) and DSSPrime.ActiveCircuit.PositiveSequence then
            begin
                if pElem.Enabled then  
                    pElem.GetCurrents(cBuffer)
                else
                    FillByte(cBuffer^, SizeOf(Complex) * MaxNValues, 0);

                Inc(iCount, 2);  // Start with kW1
                // Put only phase 1 quantities in Pos seq
                for j := 1 to pElem.NTerms do
                begin
                    k := (j - 1) * pElem.NConds;
                    if pElem.Enabled and (pElem.NodeRef <> NIL) then
                    begin
                        n := pElem.NodeRef[k + 1];
                        Vph[1] := NodeV[n];  // Get voltage at node
                        S := Vph[1] * cong(cBuffer[k + 1]);   // Compute power per phase
                        Result[icount] := S.re * 0.003; // 3-phase kW conversion
                        Result[icount + 1] := S.im * 0.003; // 3-phase kvar conversion
                    end;                    
                    Inc(icount, 6);
                end;
                Dec(iCount, 2);
            end
            else
            begin
                for i := 0 to 2 * 3 * pElem.NTerms - 1 do
                    Result[iCount + i] := -1.0;  // Signify n/A
                    
                Inc(iCount, 6 * pElem.NTerms);
            end;
        end
        else
        begin
            if pElem.Enabled then  
                pElem.GetCurrents(cBuffer)
            else
                FillByte(cBuffer^, SizeOf(Complex) * MaxNValues, 0);
        
            for j := 1 to pElem.NTerms do
            begin
                k := (j - 1) * pElem.NConds;
                if pElem.Enabled and (pElem.NodeRef <> NIL) then                
                begin
                    for i := 1 to 3 do
                        Vph[i] := NodeV[pElem.NodeRef[i + k]];
                    for i := 1 to 3 do
                        Iph[i] := cBuffer[k + i];

                    Phase2SymComp(@Iph, @I012);
                    Phase2SymComp(@Vph, @V012);

                    for i := 1 to 3 do
                    begin
                        S := V012[i] * cong(I012[i]);
                        Result[icount] := S.re * 0.003; // 3-phase kW conversion
                        Result[icount + 1] := S.im * 0.003; // 3-phase kW conversion
                        Inc(icount, 2);
                    end;
                end
                else
                    Inc(icount, 6);
            end;
        end;
        Inc(CResultPtr, 3 * pElem.NTerms);
        
    end;
    ReAllocMem(cBuffer, 0);

    if (idx_before > 0) and (idx_before <= pList.Count) then
        pList.Get(idx_before);
end;

procedure PDElements_Get_AllSeqPowers_GR(); CDECL;
// Same as PDElements_Get_AllPowers but uses global result (GR) pointers
begin
    PDElements_Get_AllSeqPowers(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0]);
end;

//------------------------------------------------------------------------------
procedure _PDElements_Get_AllNum_x(DSS: TDSSContext; var ResultPtr: PInteger; ResultCount: PAPISize; const what: Integer);
var
    idx_before, numEnabled: Integer;
    pElem: TPDElement;
    pList: TDSSPointerList;
    pval: PAPISize;
begin
    if InvalidCircuit(DSS) then
    begin
        DefaultResult(ResultPtr, ResultCount, -1);
        Exit;
    end;
    pList := DSS.ActiveCircuit.PDElements;
    if pList.Count <= 0 then
    begin
        DefaultResult(ResultPtr, ResultCount, -1);
        Exit;
    end;
    
    idx_before := pList.ActiveIndex;
    numEnabled := pList.Count;
//    for pElem in pList do
//    begin
//        if pElem.Enabled then 
//            Inc(numEnabled);
//    end;
    DSS_RecreateArray_PInteger(ResultPtr, ResultCount, numEnabled);
    pval := ResultPtr;
    
    case what of
        0:
            for pElem in pList do
            begin
                // if pElem.Enabled then
                begin
                    pval^ := pElem.NPhases;
                    Inc(pval);
                end;
            end;
        1:
            for pElem in pList do
            begin
                // if pElem.Enabled then
                begin
                    pval^ := pElem.Nconds;
                    Inc(pval);
                end;
            end;
        2:
            for pElem in pList do
            begin
                // if pElem.Enabled then
                begin
                    pval^ := pElem.Nterms;
                    Inc(pval);
                end;
            end;
    end;
        
    if (idx_before > 0) and (idx_before <= pList.Count) then
        pList.Get(idx_before);
end;
//------------------------------------------------------------------------------
procedure PDElements_Get_AllNumPhases(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    _PDElements_Get_AllNum_x(DSSPrime, ResultPtr, ResultCount, 0);
end;

procedure PDElements_Get_AllNumPhases_GR(); CDECL;
// Same as PDElements_Get_AllNumPhases but uses global result (GR) pointers
begin
    PDElements_Get_AllNumPhases(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0]);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllNumConductors(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    _PDElements_Get_AllNum_x(DSSPrime, ResultPtr, ResultCount, 1);
end;

procedure PDElements_Get_AllNumConductors_GR(); CDECL;
// Same as PDElements_Get_AllNumConductors but uses global result (GR) pointers
begin
    PDElements_Get_AllNumConductors(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0]);
end;

//------------------------------------------------------------------------------
procedure PDElements_Get_AllNumTerminals(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    _PDElements_Get_AllNum_x(DSSPrime, ResultPtr, ResultCount, 2);
end;

procedure PDElements_Get_AllNumTerminals_GR(); CDECL;
// Same as PDElements_Get_AllNumPhases but uses global result (GR) pointers
begin
    PDElements_Get_AllNumTerminals(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0]);
end;

//------------------------------------------------------------------------------

end.
