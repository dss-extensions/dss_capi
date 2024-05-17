unit NCIMSolutionHelper;

interface

uses 
    Solution,
    UComplex,
    DSSUcomplex,
    ArrayDef;

type

TNCIMSolutionHelper = class helper for TSolutionObj
public
    procedure NCIM_ApplyCurr();
    procedure NCIM_GetPowers();
    procedure NCIM_DoPVBus(i: Integer; VTarget: Double; Power: Complex);
    procedure NCIM_DoPQBus(i: Integer; V: Complex; Power: Complex);
    procedure NCIM_DoZBus(i: Integer; V: Complex; YPrim: TcMatrix);
    procedure NCIM_InitVectors();
    procedure NCIM_DoForceFlatStart();
    procedure NCIM_InitPQGen();
    procedure NCIM_DistGenClusters();
    procedure NCIM_ReversePQ2PV();
    function NCIM_Init(InitY: Boolean): Integer;
    procedure NCIM_LoadYBus();
    procedure NCIM_CalcInjCurr(NCIM_InitGenQ: Boolean);
    function NCIM_GetNumGenerators(InitQ: Boolean): Integer;
    procedure NCIM_UpdateGenQ();
    procedure NCIM_BuildJacobian();

    function NCIM_Converged(): Boolean;
    procedure DoNCIMSolution();
end;

implementation

procedure TNCIMSolutionHelper.NCIM_ApplyCurr();
// Apply the current injections before solving NCIM
var
    i: Integer;
begin
    for i := 1 to High(NCIM_NodePower) do
    begin
        if (NCIM_NodePower[i].re = 0) and (NCIM_NodePower[i].im = 0) then
            continue;

        if NCIM_NodeType[i] = PV_Node then
            NCIM_DoPVBus(i, NCIM_NodePVTarget[i], NCIM_NodePower[i])
        else
            NCIM_DoPQBus(i, NodeV[i], NCIM_NodePower[i]);
    end;
end;

procedure TNCIMSolutionHelper.NCIM_GetPowers();
// Populate the total power vector before solving
var
    pElem: TDSSCktElement;
    valid: Boolean;
    Idx,
    NodeIdx: Integer;
    LdPower,
    LdVolt,
    GenS: Complex;
    pGen: TGeneratorObj;
begin
    NodeIdx := 0;
    valid := false;
    LdPower := CZero;
    LdVolt := CZero;
    Gens := CZero;

    for pElem in PCElements do
    begin
        if (not pElem.Enabled) then
            continue;

        for Idx := 1 to pElem.NPhases do
        begin
            NodeIdx := pElem.NodeRef[idx];
            case (pElem.DSSObjType and CLASSMASK) of
                LOAD_ELEMENT:
                begin
                    LdPower := cmplx(TLoadObj(pElem).Get_WNominal, TLoadObj(pElem).Get_varNominal);
                    LdVolt := Solution.NodeV[NodeIdx];

                    if (TLoadObj(pElem).FLoadModel = 2) then
                        NCIM_DoZBus(NodeIdx, LdVolt, pElem.YPrim)
                    else
                    begin
                        if (NCIM_NodeType[NodeIdx] = PV_Node) then
                            NCIM_NodePower[NodeIdx] := NCIM_NodePower[NodeIdx] - LdPower
                        else
                            NCIM_NodePower[NodeIdx] := NCIM_NodePower[NodeIdx] + LdPower;

                        pElem.Iterminal[Idx] := cong(LdPower / LdVolt);
                    end;
                end;
                GEN_ELEMENT:
                begin
                    pGen := TGeneratorObj(pElem);

                    case pGen.GenModel of
                        3:
                        begin // Generator is a PV bus
                            pGen.GenVars.Qnominalperphase := pGen.GenVars.deltaQNom[idx - 1];

                            GenS := cmplx(pGen.GenVars.Pnominalperphase, pGen.GenVars.Qnominalperphase);
                            if (NCIM_NodeType[NodeIdx] = PQ_Node) then
                                NCIM_NodePower[NodeIdx] := cnegate(NCIM_NodePower[NodeIdx]);

                            NCIM_NodeType[NodeIdx] := PV_Node; // Forces the node to be PV
                            NCIM_NodePower[NodeIdx] := GenS + NCIM_NodePower[NodeIdx];
                            NCIM_GenPower[NodeIdx] := GenS + NCIM_GenPower[NodeIdx];

                            NCIM_NodePVTarget[NodeIdx] := pGen.GenVars.VTarget; // Updates the target for the Bus, just in case
                            NCIM_PVBusIdx[NodeIdx] := pGen.NCIM_Idx + idx; // Stores the generator IDX in the NCIM array
                        end;
                        4:
                        begin // Generator acts like PQ bus
                            LdVolt := Solution.NodeV[NodeIdx];
                            if (Length(pGen.GenVars.deltaQNom) = 0) then
                                GenS := cmplx(pGen.GenVars.Pnominalperphase, pGen.GenVars.Qnominalperphase)
                            else
                                GenS := cmplx(pGen.GenVars.Pnominalperphase, pGen.GenVars.deltaQNom[0]);

                            if (NCIM_NodeType[NodeIdx] = PQ_Node) then
                                NCIM_NodePower[NodeIdx] := NCIM_NodePower[NodeIdx] - GenS
                            else
                                NCIM_NodePower[NodeIdx] := NCIM_NodePower[NodeIdx] + GenS;

                            NCIM_GenPower[NodeIdx] := GenS + NCIM_GenPower[NodeIdx];
                        end
                    else // Constant impedance
                        begin
                            LdVolt := Solution.NodeV[NodeIdx];
                            NCIM_DoZBus(NodeIdx, LdVolt, pGen.YPrim);
                        end;
                    end;

                end;
                FAULTOBJECT:
                begin
                    LdVolt := Solution.NodeV[NodeIdx];
                    NCIM_DoZBus(NodeIdx, LdVolt, pElem.YPrim);
                end;
            else
                begin
                    // Ignore the others
                end;
            end;
        end;
    end;
end;

procedure TNCIMSolutionHelper.NCIM_DoPVBus(i: Integer; VTarget: Double; Power: Complex);
// Apply the PV bus current injection for NCIM
var
    Pow,
    FaVr,
    FaVm,
    Temp,
    Vc2,
    V,
    PowN,
    Curr: Complex;
    Vmag,
    VError,
    den,
    myVal: Double;
    GCoord,
    GCoordY: Integer;

const
    LCoords: array [0..3] of array [0..1] of Integer = ((0, 0), (1, 1), (0, 1), (1, 0));

begin
    Temp := CZero;
    Pow := cong(Power);
    V := NodeV[i];
    PowN := Power;
    Curr := cong(PowN / V);
    Vc2 := cong(V) * cong(V);
    FaVr := cmplx(-1, 0) / Vc2;
    FaVm := cmplx(0, 1) / Vc2;
    GCoord := (i * 2) - 1;
    // Updates the Jacobian

    // dImdVr
    Temp.re := -1.0 * (FaVr * Pow).im;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[0][0], GCoord + LCoords[0][1], @Temp);
    // dIrdVm
    Temp.re := -1.0 * (FaVm * Pow).re;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[1][0], GCoord + LCoords[1][1], @Temp);
    // dImdVm
    Temp.re := -1.0 * (FaVm * Pow).im;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[2][0], GCoord + LCoords[2][1], @Temp);
    // dIrdVr
    Temp.re := -1.0 * (FaVr * Pow).re;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[3][0], GCoord + LCoords[3][1], @Temp);

    // Add current injection contributions to NCIM_deltaF
    dec(GCoord); // Removes the additional index added by DSS
    NCIM_deltaF[GCoord].re := NCIM_deltaF[GCoord].re - Curr.im; // Respecting the decoupled distribution
    NCIM_deltaF[GCoord + 1].re := NCIM_deltaF[GCoord + 1].re - Curr.re; // Prioritizing reactive power over the diagonal

    // Add delta V to NCIM_deltaF in the voltage regulation subsection
    VMag := ctopolar(V).mag;
    GCoord := (ActiveCircuit.NumNodes * 2) + NCIM_PVBusIdx[i] - 1;
    VError := VTarget - VMag;
    NCIM_deltaF[GCoord - 1].re := VError;

    // Calculate the voltage regulation coefficients (Z)
    GCoordY := (i * 2) - 1;
    
    // Adds the regulation coefficients
    Temp := -1 * V.re / VMag
    SetMatrixElement(NCIM_Jacobian, GCOord, GCoordY + 0, @Temp);
    Temp := -1 * V.im / VMag;
    SetMatrixElement(NCIM_Jacobian, GCOord, GCoordY + 1, @Temp);
    // Calculate the power regulation coefficients (X)
    den := VMag * VMag;
    
    // Adds the regulation coefficients
    Temp := cong(V).re / den;
    SetMatrixElement(NCIM_Jacobian, GCoordY + 0, GCoord, @Temp);
    Temp := cong(V).im / den;
    SetMatrixElement(NCIM_Jacobian, GCoordY + 1, GCoord, @Temp);
end;

procedure TNCIMSolutionHelper.NCIM_DoPQBus(i: Integer; V: Complex; Power: Complex);
// Apply the PQ bus current injection for NCIM
var
    Pow,
    FaVr,
    FaVm,
    Temp,
    Curr,
    Vc2: Complex;
    j,
    GCoord: Integer;
const
    LCoords: array [0..3] of array [0..1] of Integer = ((0, 0), (1, 1), (0, 1), (1, 0));
begin
    Temp := CZero;
    Pow := cong(Power);
    Vc2 := cong(V) * cong(V);
    FaVr := cmplx(-1, 0) / Vc2;
    FaVm := cmplx(0, 1) / Vc2;
    Curr := cong(Power / V);
    GCoord := (i * 2) - 1;
    
    // Updates the Jacobian

    // dImdVr
    Temp.re := (FaVr * Pow).im;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[0][0], GCoord + LCoords[0][1], @Temp);
    // dIrdVm
    Temp.re := (FaVm * Pow).re;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[1][0], GCoord + LCoords[1][1], @Temp);
    // dImdVm
    Temp.re := (FaVm * Pow).im;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[2][0], GCoord + LCoords[2][1], @Temp);
    // dIrdVr
    Temp.re := (FaVr * Pow).re;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[3][0], GCoord + LCoords[3][1], @Temp);

    // Add current injection contributions to NCIM_deltaF
    dec(GCoord); // Removes the additional index added by DSS
    NCIM_deltaF[GCoord].re := NCIM_deltaF[GCoord].re + Curr.im; // Respecting the decoupled distribution
    NCIM_deltaF[GCoord + 1].re := NCIM_deltaF[GCoord + 1].re + Curr.re; // Prioritizing reactive power over the diagonal
end;

procedure TNCIMSolutionHelper.NCIM_DoZBus(i: Integer; V: Complex; YPrim: TcMatrix);
// Apply the COnstant impedance bus current injection for NCIM
var
    Curr,
    Temp: Complex;
    pYMat: pComplexArray;
    MOrder,
    GCoord: Integer;
const
    LCoords: array [0..3] of array [0..1] of Integer = ((0, 0), (1, 1), (0, 1), (1, 0));
begin
    Temp := 0;
    GCoord := (i * 2) - 1;
    pYMat := YPrim.GetValuesArrayPtr(MOrder);
    Curr := V * pYmat[1];

    // Updates the Jacobian

    // dImdVr
    Temp.re := pYMat[1].im;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[0][0], GCoord + LCoords[0][1], @Temp);
    // dIrdVm
    Temp.re := -1 * pYMat[1].im;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[1][0], GCoord + LCoords[1][1], @Temp);
    // dImdVm
    Temp.re := pYMat[1].re;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[2][0], GCoord + LCoords[2][1], @Temp);
    // dIrdVr
    Temp.re := pYMat[1].re;
    SetMatrixElement(NCIM_Jacobian, GCoord + LCoords[3][0], GCoord + LCoords[3][1], @Temp);

    // Add current injection contributions to NCIM_deltaF
    dec(GCoord); // Removes the additional index added by DSS
    NCIM_deltaF[GCoord].re := NCIM_deltaF[GCoord].re + Curr.im; // Respecting the decoupled distribution
    NCIM_deltaF[GCoord + 1].re := NCIM_deltaF[GCoord + 1].re + Curr.re; // Prioritizing reactive power over the diagonal
end;

procedure TNCIMSolutionHelper.NCIM_InitVectors();
// Initializes the vectors for the node total power in NCIM
var
    myBName: String;
    j,
    i: Integer;
begin
    SetLength(NCIM_NodePower, 1);
    SetLength(NCIM_GenPower, 1);
    SetLength(NCIM_NodeType, 1);
    SetLength(NCIM_NodePVTarget, 1);
    SetLength(NCIM_PVBusIdx, 1);
    SetLength(NCIM_NodeLimits, 1);
    SetLength(NCIM_NodeNumGen, 1);

    NCIM_NodePower[0] := CZero;
    NCIM_NodeType[0] := -1; // means ignore
    myBName := '';
    with ActiveCircuit do
    begin
        for i := 0 to (NumBuses - 1) do
        begin
            myBName := BusList.Get(i + 1);
            with Buses[i + 1] do
            begin
                for j := 0 to (NumNodesThisBus - 1) do
                begin
                    SetLength(NCIM_NodePower, Length(NCIM_NodePower) + 1);
                    NCIM_NodePower[High(NCIM_NodePower)] := 0;

                    SetLength(NCIM_GenPower, Length(NCIM_GenPower) + 1);
                    NCIM_GenPower[High(NCIM_GenPower)] := 0;

                    SetLength(NCIM_NodeType, Length(NCIM_NodeType) + 1);
                    NCIM_NodeType[High(NCIM_NodeType)] := PQ_Node; // Initially, all the buses are PQ

                    SetLength(NCIM_PVBusIdx, Length(NCIM_PVBusIdx) + 1);
                    NCIM_PVBusIdx[High(NCIM_PVBusIdx)] := 0;

                    SetLength(NCIM_NodePVTarget, Length(NCIM_NodePVTarget) + 1);
                    NCIM_NodePVTarget[High(NCIM_NodePVTarget)] := 0;

                    SetLength(NCIM_NodeLimits, Length(NCIM_NodeLimits) + 1);
                    NCIM_NodeLimits[High(NCIM_NodeLimits)] := 0;

                    SetLength(NCIM_NodeNumGen, Length(NCIM_NodeNumGen) + 1);
                    NCIM_NodeNumGen[High(NCIM_NodeNumGen)] := 0;
                end;
            end;
        end;
    end;
end;

procedure TNCIMSolutionHelper.NCIM_DoForceFlatStart();
// Forces the voltage vector to a flat start (magnitude only).
var
    TempPolar: polar;
    BaseAng,
    mykVBase: Double;
    i,
    SlackNumNodes,
    AIdx: Integer;
    pElem: TVSourceobj;

const
    myAng: array [0..2] of Double = (0.0, 4 * Pi / 3, 2 * Pi / 3);

begin
    // Sets the initial solution using the calculated angles and the buses voltage bases
    TempPolar := ctopolar(CZero);
    mykVBase := 0.0;
    Aidx := 0;

    // Ignores the nodes attached to the slack bus
    SlackNumNodes := 1;
    for i := SlackNumNodes to ActiveCircuit.NumNodes do
    begin
        mykVBase := Buses[MapNodeToBus[i].BusRef].kVBase * 1e3;
        TempPolar := ctopolar(NodeV[i]);
        TempPolar.mag := mykVBase;
        TempPolar.ang := myAng[AIdx];
        NodeV[i] := ptocomplex(TempPolar);
        inc(AIdx);
        if AIdx >= 3 then
            AIdx := 0;
    end;
    // Now add the slack bus data
    pElem := CktElements.First;
    TempPolar.mag := ((pElem.kVBase * 1e3) / SQRT3) * pElem.PerUnit;
    BaseAng := pElem.Angle * Pi / 180;
    for i := 1 to 3 do
    begin
        TempPolar.ang := ((pElem.Angle * Pi) / 180) + myAng[i - 1];
        NodeV[i] := ptocomplex(TempPolar);
    end;
end;

procedure TNCIMSolutionHelper.NCIM_InitPQGen();
// Initializes the generators declared as PQ type
// 
// Initializes the registries for generators declared as PQ buses (Mode 4) by loading up their deltaQ
// with the q nominal per phase given at the generator's declaration
var
    pGen: TGeneratorObj;
begin
    for pGen in ActiveCircuit.Generators do
    begin
        if (pGen.Enabled) and (pGen.GenModel <> 3) then
        begin
            SetLength(pGen.GenVars.deltaQNom, 1);
            pGen.GenVars.deltaQNom[0] := pGen.GenVars.Qnominalperphase;
        end;
    end;
end;

procedure TNCIMSolutionHelper.NCIM_DistGenClusters();
// Distributes the reactive power among clustered generators
var
    pGen: TGeneratorObj;
    j: Integer;
    Volt: Complex;
    Qlocal: Double;
begin
    for pGen in ActiveCircuit.Generators do
    begin
        if not pGen.Enabled then
            continue;

        if (NCIM_NodeNumGen[pGen.NodeRef[1]] > 1) and ((pGen.GenModel = 3) or (pGen.GenModel = 4)) then
        begin
            for j := 1 to pGen.NPhases do
            begin
                Qlocal := Abs(pGen.GenVars.Pnominalperphase / NCIM_GenPower[pGen.NodeRef[j]].re) * NCIM_GenPower[pgen.NodeRef[j]].im;

                Volt := NodeV[pGen.NodeRef[j]];
                pGen.Iterminal[j] := -cong(cmplx(pGen.GenVars.Pnominalperphase, Qlocal) / Volt);
            end;
        end;
    end;
end;

procedure TNCIMSolutionHelper.NCIM_ReversePQ2PV();
// Reverses the generators converted from PV 2 PQ for the next solution
// 
// Reverses to model 3 all the generators turned into model 4 automatically when the option AvoidPV2PQ is
// disabled, this to take the model back to its original values after these type of changes take place
var
    pGen: TGeneratorObj;
begin
    for pGen in ActiveCircuit.Generators do
    begin
        if (Flg.NCIM_ExPV in pGen.Flags) then
        begin
            pGen.GenModel := 3;
            Exclude(Flg.NCIM_ExPV, pGen.Flags);
        end;
    end;
end;

function TNCIMSolutionHelper.NCIM_Init(InitY: Boolean): Integer;
// Hosts all the initialization routines for NCIM
var
    i: Integer;
    NNodes: Longword;
begin
    with ActiveCircuit do
    begin
        // 1. Calculate the Y Bus, PDE only
        BuildYMatrix(PDE_ONLY, false); // Does not realloc V, I
        NCIM_InitVectors();
        // 2. Performs a flat solution to get the initial voltage estimation
        ZeroInjCurr(); // All to 0
        GetSourceInjCurrents(); // sources
        // Solve for voltages 
        // Note:NodeV[0] = 0 + j0 always
        if (LogEvents) then
            LogThisEvent('Solve Sparse Set DoNCIMSolution ...');

        if InitY then
        begin
            // Estimate the initial values for the solution
            SolveSystem(NodeV);
            // 3. Move the Y bus matrix into its sparse lib equivalent for linear algebra ops
            NCIM_DoForceFlatStart();
        end;
        // Gets the number of buses for the system
        GetSize(hY, @NNodes);

        // 4. Setup the Y admittance matrix equivalent for lienar algebra orperations
        NCIM_LoadYBus();
        NCIM_Ready := true;
    end;
    Result := NNodes;
end;

procedure TNCIMSolutionHelper.NCIM_LoadYBus();
// Loads the Y bus admittance matrix into another structure for linear algebra purposes
var
    NBus,
    nNZ: Longword;
    ColPtr,
    RowIdx: array of Integer;
    cVals: array of Complex;
    re,
    im: Double;
    col,
    Row,
    myhY: Nativeuint;
begin
    if ASSIGNED(ActiveCircuit) then
    begin
        myhY := hY;
        if myhY = 0 then
            DoSimpleMsg('Y Matrix not Built.', 222)
        else
        begin
            // this compresses the entries if necessary - no extra work if already solved
            FactorSparseMatrix(myhY);
            GetNNZ(myhY, @nNZ);
            GetSize(myhY, @NBus); // we should already know this

            SetLength(NCIM_YCol, nNZ);
            SetLength(NCIM_YRow, nNZ);
            SetLength(NCIM_Y, nNZ);
            GetTripletMatrix(myhY, nNZ, @(NCIM_YRow[0]), @(NCIM_YCol[0]), @(NCIM_Y[0]));
        end;
    end;
end;

procedure TNCIMSolutionHelper.NCIM_CalcInjCurr(NCIM_InitGenQ: Boolean);
// Calculates the injection currents using the actual voltages ( I = Y * V )
// 
// Calculates the injection currents based on the voltages at the nodes using
// I = YE, this is later used for estimating the convergence in terms of power
var
    NBus: Longint;
    myvalue: Complex;
    i,
    GSize: Integer;
begin
    GSize := (NCIM_Nodes * 2) + NCIM_GetNumGenerators(NCIM_InitGenQ);

    // 4. Resize the input/output vectors
    SetLength(NCIM_deltaF, GSize); // Resizes the InjCurr mismatch vector to host also voltage control
    SetLength(NCIM_deltaZ, GSize); // Resizes the voltage mismatch vector including delta Q spaces

    for i := 0 to (GSize - 1) do
        NCIM_deltaF[i] := CZero;

    // Multiplies the latest solution (V) by the Y Matrix
    for i := 0 to (Length(NCIM_Y) - 1) do
    begin
    // First the value found
        myvalue := NCIM_Y[i] * NodeV[NCIM_YCol[i] + 1];
        NCIM_deltaF[NCIM_YRow[i] * 2].re := NCIM_deltaF[NCIM_YRow[i] * 2].re + myvalue.im;
        NCIM_deltaF[(NCIM_YRow[i] * 2) + 1].re := NCIM_deltaF[(NCIM_YRow[i] * 2) + 1].re + myvalue.re;
    end;

    // The first 6 elements are equal to 0
    for i := 0 to 5 do
        NCIM_deltaF[i] := CZero;
end;

function TNCIMSolutionHelper.NCIM_GetNumGenerators(InitQ: Boolean): Integer;
// Gets and initializes all the generators in the model as PV buses
// 
// Gets the number of generators in the modeland their number of phases
// Returns the number of generators times their number of phases
// This form allocating memory within the Jacobian matrix for voltage control (PV buses)
// Use it ONLY for initializing the structures within the NCIM algorithm
var
    pGen: TGeneratorObj;
    Idx,
    i,
    k,
    BIdx: Integer;
    BusRefs: array of Integer;
    qMax,
    qMin: Double;
    Add2Limits: Boolean;
    j: Integer;

begin
    BIdx := 0;
    qMax := 0.0;
    qMin := 0.0;
    Add2Limits := false;
    Result := 0;

    SetLength(BusRefs, 0);
    for Idx := 0 to High(NCIM_NodeNumGen) do
    begin
        NCIM_NodeNumGen[Idx] := 0;
        NCIM_NodeLimits[Idx] := CZero;
    end;

    if (NumGens <= 0) then
        Exit;

    i := -1;
    for pGen in ActiveCircuit.Generators do
    begin
        inc(i);
        if not pGen.Enabled then
            continue;

        Add2Limits := false;

        qMax := (pGen.kvarMax * 1e3) / pGen.NPhases; // Stores the Q limits for further use
        qMin := (pGen.kvarMin * 1e3) / pGen.NPhases;

        // if (pGen.GenModel = 3) and (not (Flg.NCIM_ExPV in pGen.Flags)) then
        if (pGen.GenModel = 3) then
        begin
            if InitQ then
            begin
                SetLength(pGen.GenVars.deltaQNom, pGen.NPhases);
                for k := 0 to (pGen.NPhases - 1) do
                    pGen.GenVars.deltaQNom[k] := 0.0; // Initializes delta Q = 0 for all the generators (PV buses)
            end;

            if ((pGen.kvarMax = 0) and (pGen.kvarMin = 0)) then
            begin
                // DSS-Extensions:
                // Changing the actual model shouldn't be required, but left to ensure compatibility
                // We could remove it later for a cleaner implementation (we need to update the checks elsewhere too)
                pGen.GenModel := 4;

                Include(Flg.NCIM_ExPV, pGen.Flags);
                continue;
            end;

            // It'll be used later by the generator to locate its voltage control signals (PV bus)
            
            // Search for the BusRef in the list, aiming at gens connected to the same bus
            BIdx := -1;
            for k := 0 to High(BusRefs) do
            begin
                if BusRefs[k] = pGen.NodeRef[1] then
                begin
                    BIdx := k;
                    break
                end;
            end;
            // Now if the active generator is not in the list
            if (BIdx < 0) then
            begin
                pGen.NCIM_Idx := Result + 1;
                Result := Result + pGen.NPhases;
                for j := 1 to pGen.NPhases do
                begin
                    SetLength(BusRefs, Length(BusRefs) + 1);
                    BusRefs[High(BusRefs)] := pGen.NodeRef[j];
                end;
            end
            else
                pGen.NCIM_Idx := BIdx + 1;

            Add2Limits := true;
        end
        else
            Add2Limits := (pGen.GenModel = 4) or ((pGen.GenModel = 3) and (Flg.NCIM_ExPV in pGen.Flags));

        if Add2Limits then
        begin
            for j := 1 to pGen.NPhases do
            begin
                NCIM_NodeLimits[pGen.NodeRef[j]] := NCIM_NodeLimits[pGen.NodeRef[j]] + cmplx(qMax, qMin);
                inc(NCIM_NodeNumGen[pGen.NodeRef[j]]);
            end;
        end;
    end;
end;

procedure TNCIMSolutionHelper.NCIM_UpdateGenQ();
// Updates the reactive power delta for all the generators in the model.
// Updates the reactive power delivery for generators model 3, this will be reflected in the next solution step.
var
    pGen: TGeneratorObj;
    GenIdx, // Index of the generator within the node space
    Shift, // shift of the Q delta within the solution space
    i,
    k,
    j,
    Checked,
    BIdx: Integer;
    Volt: Complex; // Votlage at the generator's terminals (per phase)
    QDelta: array of Double; // Vector to copy NCIM_deltaZ and assign Q updates incrementally
    qMax, // For storing the Q max limit of the active generator
    qMin, // For storing the Q min limit of the active generator
    GenQ, // Temporary register for storing the unbound expected Q for the active generator
    VNode, // To remporarily store the voltage at the active Node
    myVMax: Double; // Stores the active generator's scheduled voltage
    myPVOK,
    isPQOK: Boolean;
    qNodeRef,
    qNodeRefPQ,
    IdxTmp: array of Integer;
    PQChecked: array of Integer;
begin
    if ActiveCircuit.Generators.ListSize = 0 then
        Exit;

    GenIdx := ActiveCircuit.NumNodes * 2;
    SetLength(QDelta, 1);
    QDelta[High(QDelta)] := 0; // leaves the first one as zero, to avoid subtractions in the below

    for i := GenIdx to High(NCIM_deltaZ) do
    begin
        SetLength(QDelta, Length(QDelta) + 1);
        QDelta[High(QDelta)] := -1 * NCIM_deltaZ[i].re; // Moves NCIM_deltaZ (only delta Q section) into the backup vector
    end;

    SetLength(qNodeRef, 0);
    SetLength(qNodeRefPQ, 0);
    SetLength(PQChecked, 0);
    pGen := Generators.First;

    for pGen in ActiveCircuit.Generators do
    begin
        if not pGen.Enabled then
            continue;

        if (pGen.GenModel = 3) then
        begin
            myPVOK := true;
            // Search for the qNodeRef in the list, aiming at gens connected to the same bus
            BIdx := -1;
            for k := 0 to High(qNodeRef) do
            begin
                if qNodeRef[k] = pGen.NodeRef[1] then
                begin
                    BIdx := k;
                    break
                end;
            end;

            if (BIdx < 0) then
            begin
                for j := 0 to (pGen.NPhases - 1) do
                begin
                    qMax := NCIM_NodeLimits[pGen.NodeRef[j + 1]].re; // gets the upper kvar limit per phase
                    qMin := NCIM_NodeLimits[pGen.NodeRef[j + 1]].im; // gets the lower kvar limit per phase
                    // Update the current at the gnerator's terminal for reporting purposes

                    Volt := NodeV[pGen.NodeRef[j + 1]];
                    // Updates Q per generator
                    Shift := pGen.NCIM_Idx + j;
                    GenQ := pGen.GenVars.deltaQNom[j] + (QDelta[Shift] * NCIM_GenGain);

                    if (not NCIM_IgnoreQLimit) then
                    begin
                        if (GenQ >= 0) then
                            myPVOK := myPVOK and (GenQ < qMax)
                        else
                            myPVOK := myPVOK and (GenQ > qMin);
                    end
                    else
                    begin
                        if ((pGen.kvarMax = 0) and (pGen.kvarMin = 0)) then // this if the limits are 0
                            GenQ := 0;
                    end;

                    QDelta[Shift] := 0.0;
                    pGen.GenVars.deltaQNom[j] := GenQ;
                    pGen.Iterminal[j + 1] := -cong(cmplx(pGen.GenVars.Pnominalperphase, pGen.GenVars.deltaQNom[j]) / Volt);
                end;
            end
            else
                myPVOK := false;

            // Changes the model type for generator if needed
            if not myPVOK then
            begin
                pGen.GenModel := 4; // If exceeds the limits changes the generator to model 4 (PQ bus)
                Include(Flg.NCIM_ExPV, pGen.Flags);

                with pGen.GenVars do
                begin
                    if BIdx < 0 then
                    begin 
                        // add all the node refs to the temp array if not there already
                        for j := 1 to pGen.NPhases do
                        begin
                            SetLength(qNodeRef, Length(qNodeRef) + 1);
                            qNodeRef[High(qNodeRef)] := pGen.NodeRef[j];
                        end;
                        qMax := NCIM_NodeLimits[pGen.NodeRef[1]].re; // gets the upper kvar limit per phase
                        qMin := NCIM_NodeLimits[pGen.NodeRef[1]].im; // gets the lower kvar limit per phase
                    end
                    else
                    begin
                        qMax := 0;
                        qMin := 0;
                    end;

                    for j := 0 to (pGen.NPhases - 1) do
                    begin
                        if deltaQNom[0] >= 0 then // and fixes the values for the next solution try
                            deltaQNom[j] := qMax
                        else
                            deltaQNom[j] := qMin;
                    end;
                end;
            end;
        end;

        if (pGen.GenModel = 4) then
        begin
            if ((pGen.kvarMax <> 0) and (pGen.kvarMin <> 0)) then
            begin
                isPQOK := true;
                // Search for the qNodeRef in the list, aiming at gens already converted to PQ
                BIdx := -1;
                for k := 0 to High(qNodeRefPQ) do
                begin
                    if qNodeRefPQ[k] = pGen.NodeRef[1] then
                    begin
                        BIdx := k;
                        break
                    end;
                end;

                if BIdx < 0 then
                begin
                    Checked := -1;
                    for k := 0 to High(PQChecked) do
                    begin
                        if PQChecked[k] = pGen.NodeRef[1] then
                        begin
                            Checked := k;
                            break
                        end;
                    end;

                    if Checked < 0 then
                    begin
                        myVMax := pGen.Get_VBase * pGen.Vpu;
                        for j := 0 to (pGen.NPhases - 1) do
                        begin
                            Volt := NodeV[pGen.NodeRef[j + 1]];
                            VNode := ctopolar(Volt).mag;
                            if pGen.GenVars.deltaQNom[0] > 0 then
                                isPQOK := isPQOK and (VNode <= myVMax)
                            else
                                isPQOK := isPQOK and (VNode >= myVMax);

                            SetLength(PQChecked, Length(PQChecked) + 1);
                            PQChecked[High(PQChecked)] := pGen.NodeRef[j + 1];
                        end;
                    end;
                end
                else
                    isPQOK := false; // belongs to a cluster and needs to be changed

                // this in case we need to go back to PV
                if not isPQOK then
                begin
                    pGen.GenModel := 3;
                    for j := 0 to (pGen.NPhases - 1) do
                    begin
                        if BIdx < 0 then
                        begin
                            qMax := NCIM_NodeLimits[pGen.NodeRef[j + 1]].re; // gets the upper kvar limit per phase
                            qMin := NCIM_NodeLimits[pGen.NodeRef[j + 1]].im; // gets the lower kvar limit per phase
                            SetLength(qNodeRefPQ, Length(qNodeRefPQ) + 1);
                            qNodeRefPQ[High(qNodeRefPQ)] := pGen.NodeRef[j + 1];
                        end
                        else
                        begin
                            qMax := 0; // If it's part of a cluster it needs to inject only P
                            qMin := 0;
                        end;

                        if pGen.GenVars.deltaQNom[0] >= 0 then // and fixes the values for the next solution try
                            pGen.GenVars.deltaQNom[j] := qMax
                        else
                            pGen.GenVars.deltaQNom[j] := qMin;
                    end;

                    if (Flg.NCIM_ExPV in pGen.Flags) then
                    begin
                        Exclude(Flg.NCIM_ExPV, pGen.Flags);
                    end;
                end;
            end;
        end;

        // Update currents for all the other gen models
        for j := 0 to (pGen.NPhases - 1) do
        begin
            Volt := NodeV[pGen.NodeRef[j + 1]];
            pGen.Iterminal[j + 1] := -cong(cmplx(pGen.GenVars.Pnominalperphase, pGen.GenVars.deltaQNom[0]) / Volt);
        end;
    end;
end;

procedure TNCIMSolutionHelper.NCIM_BuildJacobian();
// Builds the Jacobian matrix using the data already allocated within the Y Bus matrix
var
    Values: array [0..3] of Double;
    i,
    j,
    GRow,
    GCol: Integer;
    myValue: Complex;
    pGen: TGeneratorObj;
const
    GCoords: array [0..3] of array [0..1] of Integer = ((0, 0), (0, 1), (1, 0), (1, 1));
begin
    //Initialization
    for i := 0 to High(Values) do
        Values[i] := 0;
    myValue := CZero;
    GRow := 0;
    GCol := 0;

    if (NCIM_Jacobian <> 0) then
    begin
        DeleteSparseSet(NCIM_Jacobian);
        NCIM_Jacobian := 0;
    end;

    NCIM_Jacobian := NewSparseSet(Length(NCIM_deltaF));
    for i := 0 to High(NCIM_Y) do
    begin
        GRow := NCIM_YRow[i] * 2;
        GCol := NCIM_YCol[i] * 2;
        if ((GRow = GCol) and (GRow < 6)) then
        begin
            // This is a diagonal for the swing bus, always 1
            myValue.re := 1;
            inc(GRow); // Needed to match with the indexes within the library
            inc(GCol);
            SetMatrixElement(NCIM_Jacobian, GRow, GCol, @myValue);
            SetMatrixElement(NCIM_Jacobian, GRow + 1, GCol + 1, @myValue);
        end
        else
        begin
            if ((GRow >= 6) and (GCol >= 6)) then // Elements beyond the swing bus
            begin
                Values[0] := NCIM_Y[i].im; // B
                Values[1] := NCIM_Y[i].re; // G
                Values[2] := NCIM_Y[i].re; // G
                Values[3] := -1 * NCIM_Y[i].im; // -B
                inc(GRow);
                inc(GCol); // Needed to match with the indexes within the library
                for j := 0 to 3 do
                begin
                    myValue.re := Values[j];
                    SetMatrixElement(NCIM_Jacobian, (GCoords[j][0] + GRow), (GCoords[j][1] + GCol), @myValue);
                end;
            end;
        end;
    end;

    // Add the Voltage regulation cells to the Jacobian for later use by PV buses
    // Update 03/05/2024 - not needed any more
    NumGens := ActiveCircuit.Generators.ListSize;
    for pGen in ActiveCircuit.Generators do
    begin
        if ((pGen.Enabled) and (pGen.GenModel = 3)) then
            pGen.InitPVBusJac();
    end;

    // Clears the total power vector
    for j := 0 to High(NCIM_NodePower) do
    begin
        NCIM_NodePower[j] := CZero;
        NCIM_GenPower[j] := CZero;
        NCIM_NodeType[j] := PQ_Node;
    end;
end;

procedure TNCIMSolutionHelper.DoNCIMSolution();
// Implements the N conductor current injection method (NCIM) for solving the power flow problem.
// 
// This mehtod is a Newton-Raphson like solution method, and is implemented here to address
// transmission system-like simulations. For more info, check:
// 
// https://www.sciencedirect.com/science/article/abs/pii/S0142061512004310
var
    dVIdx,
    i: Integer;
    Solved: Boolean;
    dV: Complex;
begin
    Iteration := 0; // Initializes iteration counter

    if NCIM_InitGenQ then 
    begin
        // If the system needs to be initialized
        
        NCIM_InitPQGen(); // Initialize PQ like generators
        setLength(PV2PQList, 0);
    end;

    if (SystemYChanged or not NCIM_Ready) then
        NCIM_Nodes := NCIM_Init(NCIM_InitGenQ); // Initializes the NCIM environment vars and structures (takes time)

    // Main iteration loop
    repeat
        inc(Iteration);
        NCIM_CalcInjCurr(NCIM_InitGenQ); // Calc Injection currents using the latest solution ( I = Y * V )
        NCIM_BuildJacobian(); // Resets the jacobian's diagonal for the next iteration
        NCIM_GetPowers(); // Populate the total power vector
        NCIM_ApplyCurr(); // Adjust Jacobian and populate the currents vector

        if ActiveCircuit.LogEvents then
            LogThisEvent('Solve Power flow DoNCIMSolution ...');

        // Solves the Jacobian
        SolveSparseSet(NCIM_Jacobian, @NCIM_deltaZ[0], @NCIM_deltaF[0]);

        //Updates the Voltage vector
        for i := 1 to ActiveCircuit.NumNodes do
        begin
            dVIdx := (i - 1) * 2;
            dV := cmplx(NCIM_deltaZ[dvIdx].re, NCIM_deltaZ[dVIdx + 1].re);
            NodeV[i] := NodeV[i] - dV;
        end;

        Solved := Converged();
        // Updates the Generator's Q using the calculated deltaQ
        NCIM_UpdateGenQ();
        NCIM_InitGenQ := false;
    until (Solved and (Iteration >= MinIterations)) or (Iteration >= MaxIterations);
end;

function TNCIMSolutionHelper.NCIM_Converged(): Boolean;
begin
    Result := false; // DSS-Extensions: this should match the original (non-init'ed) behavior
    for i := 0 to High(NCIM_deltaF) do
    begin
        Result := Abs(NCIM_deltaF[i].re) <= ConvergenceTolerance;
        if not Result then
            break;
    end;
    ConvergedFlag := Result;
end;

end.