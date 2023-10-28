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
    DSSHelper,
    CAPI_Alt;

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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_SeqVoltages(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_Voltages(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_Nodes(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_Isc(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_Voc(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_puVoltages(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    Alt_Bus_Get_ZscMatrix(DSSPrime, ResultPtr, ResultCount, pBus);
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

    Result := (DSSPrime.DSSExecutive.DoZscRefresh = 0);
end;
//------------------------------------------------------------------------------
procedure Bus_Get_YscMatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pBus: TDSSBus;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pBus) then
        Exit;
    Alt_Bus_Get_YscMatrix(DSSPrime, ResultPtr, ResultCount, pBus);
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

    Result := DSSPrime.ActiveCircuit.GetUniqueNodeNumber(pBus, StartNumber);
end;
//------------------------------------------------------------------------------
procedure Bus_Get_CplxSeqVoltages(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
// Compute sequence voltages for Active Bus
// Complex values
// returns a set of seq voltages (3) in 0, 1, 2 order
var
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_ComplexSeqVoltages(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_puVLL(DSSPrime, ResultPtr, ResultCount, pBus);
end;

procedure Bus_Get_puVLL_GR(); CDECL;
// Same as Bus_Get_puVLL but uses global result (GR) pointers
begin
    Bus_Get_puVLL(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Bus_Get_VLL(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_VLL(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_puVMagAngle(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_VMagAngle(DSSPrime, ResultPtr, ResultCount, pBus);
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
    pBus: TDSSBus;
begin
    if not _activeObj(DSSPrime, pBus) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Alt_Bus_Get_Zsc012Matrix(DSSPrime, ResultPtr, ResultCount, pBus);
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
