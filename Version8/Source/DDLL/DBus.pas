unit DBus;

interface

function BUSI(mode: Longint; arg: Longint): Longint; CDECL;
function BUSF(mode: Longint; arg: Double): Double; CDECL;
function BUSS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure BUSV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Circuit,
    Ucomplex,
    MathUtil,
    sysutils,
    ExecHelper,
    SolutionAlgs,
    Variants,
    Utilities,
    Bus,
    CktElement,
    Ucmatrix,
    Arraydef;

function CheckBusReference(cktElem: TDSSCktElement; BusReference: Integer; var TerminalIndex: Integer): Boolean;

{Check all terminals of cktelement to see if bus connected to busreference}

var
    i: Integer;
begin
    Result := FALSE;
    with cktElem do
        for i := 1 to NTerms do
        begin
            if Terminals^[i].BusRef = BusReference then
            begin
                TerminalIndex := i;
                Result := TRUE;
                Break;
            end;
        end;
end;

function BUSI(mode: Longint; arg: Longint): Longint; CDECL;

begin
    Result := 0;  // Default return value
    case mode of
        0:
        begin                                           // Bus.NumNodes
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].NumNodesThisBus;
        end;
        1:
        begin                                           // Bus.ZscRefresh
            Result := 0;   // Init in case of failure
            if ExecHelper.DoZscRefresh(ActiveActor) = 0 then
                Result := 1;
        end;
        2:
        begin                                           // Bus.Coorddefined
            Result := 0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                            Result := 1;
        end;
        3:
        begin                                           // Bus.GetUniqueNodeNumber
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Utilities.GetUniqueNodeNumber(BusList.Get(ActiveBusIndex), arg);
        end;
        4:
        begin                                           // Bus.N_Customers
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].BusTotalNumCustomers;
        end;
        5:
        begin                                           // Bus.SectionID
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].BusSectionID;
        end
    else
        Result := -1;
    end;
end;

//**************************floating point variables***************************
function BUSF(mode: Longint; arg: Double): Double; CDECL;
begin
    Result := 0.0;  // Default return value
    case mode of
        0:
        begin                                           // Bus.kVBase
            Result := 0.0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].kVBase;
        end;
        1:
        begin                                           // Bus.X -read
            Result := 0.0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                            Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x;
        end;
        2:
        begin                                           // Bus.X - Write
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
                        Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x := arg;
                    end;
            Result := 0.0;
        end;
        3:
        begin                                           // Bus.Y -read
            Result := 0.0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                            Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].y;
        end;
        4:
        begin                                           // Bus.Y - Write
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Buses^[ActiveBusIndex].Coorddefined := TRUE;
                        Buses^[ActiveBusIndex].y := arg;
                    end;
            Result := 0.0;
        end;
        5:
        begin                                           // Bus.Distance
            Result := 0.0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if ((ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses)) then
                        Result := Buses^[ActiveBusIndex].DistFromMeter;
        end;
        6:
        begin                                           // Bus.Lambda
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].BusFltRate;
        end;
        7:
        begin                                           // Bus.N_interrupts
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].Bus_Num_Interrupt;
        end;
        8:
        begin                                           // Bus.int_duration
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].Bus_Int_Duration;
        end;
        9:
        begin                                           // Bus.Cust_interrupts
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].BusCustDurations;
        end;
        10:
        begin                                          // Bus.Cust_duration
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].BusCustDurations;
        end;
        11:
        begin                                          // Bus.Totalmiles
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if ActiveBusIndex > 0 then
                        Result := Buses^[ActiveBusIndex].BusTotalMiles;
        end;
        12:
        begin                                          // Bus.latitude read
            Result := 0.0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                            Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
        end;
        13:
        begin                                          // Bus.Latitude write
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
                        Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat := arg;
                    end;
        end;
        14:
        begin                                          // Bus.latitude read
            Result := 0.0;
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                            Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
        end;
        15:
        begin                                          // Bus.Latitude write
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
                        Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long := arg;
                    end;
        end
    else
        Result := -1.0;
    end;
end;

//*****************************String type properties*******************************
function BUSS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
begin
    Result := pAnsiChar(Ansistring('0')); //Default return value
    case mode of
        0:
        begin                                           // Bus.Name read
            Result := pAnsiChar(Ansistring(''));

            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        Result := pAnsiChar(Ansistring(BusList.Get(ActiveBusIndex)));
        end
    else
        Result := pAnsiChar(Ansistring('Error, Parameter non recognized'));
    end;
end;

procedure BUSV(mode: Longint; out arg: Variant); CDECL;

var
    BusReference,
    k,
    LoadCount,
    LineCount,
    Nvalues,
    Norder,
    i,
    iV,
    NodeIdx,
    jj,
    NodeIdxj,
    Nelements,
    j,
    NodeIdxi: Integer;
    Volts,
    Voc,
    Z,
    Y1,
    Isc: Complex;
    pBus: TDSSBus;
    VPh,
    V012: array[1..3] of Complex;
    BaseFactor: Double;
    voltsp: polar;
    pElem: TDSSCktElement;
    Zsc012Temp: TCmatrix;
    pValues: pDoubleArray;
    myPXEList: DynStringArray;

begin
    case mode of
        0:
        begin                                           // Bus.Voltages
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                        iV := 0;
                        jj := 1;
                        with pBus do
                            for i := 1 to NValues do
                            begin
                // this code so nodes come out in order from smallest to larges
                                repeat
                                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                                    inc(jj)
                                until NodeIdx > 0;
                                Volts := Solution.NodeV^[GetRef(NodeIdx)];  // referenced to pBus
                                arg[iV] := Volts.re;
                                Inc(iV);
                                arg[iV] := Volts.im;
                                Inc(iV);
                            end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        1:
        begin                                           // Bus.SeqVoltages
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
                        if Nvalues > 3 then
                            Nvalues := 3;
          // Assume nodes 1, 2, and 3 are the 3 phases
                        arg := VarArrayCreate([0, 2], varDouble);
                        if Nvalues <> 3 then
                            for i := 1 to 3 do
                                arg[i - 1] := -1.0  // Signify seq voltages n/A for less then 3 phases
                        else
                        begin
                            iV := 0;
                            for i := 1 to 3 do
                            begin
                                Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
                            end;
                            Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components
                            for i := 1 to 3 do  // Stuff it in the result
                            begin
                                arg[iV] := Cabs(V012[i]);
                                Inc(iV);
                            end;
                        end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);
        end;
        2:
        begin                                           // Bus.Nodes
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varInteger)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        with pBus do
                        begin
                            Nvalues := NumNodesThisBus;
                            arg := VarArrayCreate([0, NValues - 1], varInteger);
                            iV := 0;
                            jj := 1;
                            for i := 1 to NValues do
                            begin
                    // this code so nodes come out in order from smallest to larges
                                repeat
                                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                                    inc(jj)
                                until NodeIdx > 0;
                                arg[iV] := Buses^[ActiveBusIndex].GetNum(NodeIdx);
                                Inc(iV);
                            end;
                        end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varInteger);  // just return null array
        end;
        3:
        begin                                           // Bus.Voc
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        if Buses^[ActiveBusIndex].VBus <> NIL then
                        begin
                            NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
                            arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                            iV := 0;
                            for i := 1 to NValues do
                            begin
                                Voc := Buses^[ActiveBusIndex].VBus^[i];
                                arg[iV] := Voc.Re;
                                Inc(iV);
                                arg[iV] := Voc.Im;
                                Inc(iV);
                            end;
                        end
                        else
                            arg := VarArrayCreate([0, 0], varDouble);
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        4:
        begin // Bus.Isc
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        if Buses^[ActiveBusIndex].BusCurrent <> NIL then
                        begin
                            NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
                            arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                            iV := 0;
                            for i := 1 to NValues do
                            begin
                                Isc := Buses^[ActiveBusIndex].BusCurrent^[i];
                                arg[iV] := Isc.Re;
                                Inc(iV);
                                arg[iV] := Isc.Im;
                                Inc(iV);
                            end;
                        end
                        else
                            arg := VarArrayCreate([0, 0], varDouble);
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        5:
        begin  // Bus.PuVoltages
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        with pBus do
                        begin
                            Nvalues := NumNodesThisBus;
                            arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                            iV := 0;
                            jj := 1;
                            if kVBase > 0.0 then
                                BaseFactor := 1000.0 * kVBase
                            else
                                BaseFactor := 1.0;
                            for i := 1 to NValues do
                            begin
                    // this code so nodes come out in order from smallest to larges
                                repeat
                                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                                    inc(jj)
                                until NodeIdx > 0;
                                Volts := Solution.NodeV^[GetRef(NodeIdx)];
                                arg[iV] := Volts.re / BaseFactor;
                                Inc(iV);
                                arg[iV] := Volts.im / BaseFactor;
                                Inc(iV);
                            end;
                        end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        6:
        begin  // Bus.ZscMatrix
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                try
                    with ActiveCircuit[ActiveActor] do
                        if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        begin
                            if Assigned(Buses^[ActiveBusIndex].Zsc) then
                            begin
                                Nelements := Buses^[ActiveBusIndex].Zsc.Order;
                                arg := VarArrayCreate([0, ((2 * Nelements * Nelements) - 1)], varDouble);
                                iV := 0;
                                with Buses^[ActiveBusIndex] do
                                    for i := 1 to Nelements do
                                        for j := 1 to Nelements do
                                        begin
                                            Z := Zsc.GetElement(i, j);
                                            arg[iV] := Z.Re;
                                            Inc(iV);
                                            arg[iV] := Z.Im;
                                            Inc(iV);
                                        end;
                            end
                            else
                                arg := VarArrayCreate([0, 0], varDouble);  // just return null array
                        end
                        else
                            arg := VarArrayCreate([0, 0], varDouble);  // just return null array
                except
                    On E: Exception do
                        DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5016);
                end;
        end;
        7:
        begin  // Bus.Zcs1
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Z := Buses^[ActiveBusIndex].Zsc1;
                        arg := VarArrayCreate([0, 1], varDouble);
                        arg[0] := Z.Re;
                        arg[1] := Z.Im;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        8:
        begin  // Bus.Zsc0
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Z := Buses^[ActiveBusIndex].Zsc0;
                        arg := VarArrayCreate([0, 1], varDouble);
                        arg[0] := Z.Re;
                        arg[1] := Z.Im;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        9:
        begin   // Bus.YscMatrix
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                try
                    with ActiveCircuit[ActiveActor] do
                        if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        begin
                            if Assigned(Buses^[ActiveBusIndex].Ysc) then
                            begin
                                Nelements := Buses^[ActiveBusIndex].Ysc.Order;
                                arg := VarArrayCreate([0, ((2 * Nelements * Nelements) - 1)], varDouble);
                                iV := 0;
                                with Buses^[ActiveBusIndex] do
                                    for i := 1 to Nelements do
                                        for j := 1 to Nelements do
                                        begin
                                            Y1 := Ysc.GetElement(i, j);
                                            arg[iV] := Y1.Re;
                                            Inc(iV);
                                            arg[iV] := Y1.Im;
                                            Inc(iV);
                                        end;
                            end
                            else
                                arg := VarArrayCreate([0, 0], varDouble);  // just return null array
                        end
                        else
                            arg := VarArrayCreate([0, 0], varDouble);  // just return null array
                except
                    On E: Exception do
                        DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5017);
                end;
        end;
        10:
        begin  // Bus.CplxSeqVoltages
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
                        if Nvalues > 3 then
                            Nvalues := 3;
          // Assume nodes labelled 1, 2, and 3 are the 3 phases
                        arg := VarArrayCreate([0, 5], varDouble);
                        if Nvalues <> 3 then
                            for i := 1 to 6 do
                                arg[i - 1] := -1.0  // Signify seq voltages n/A for less then 3 phases
                        else
                        begin
                            iV := 0;
                            for i := 1 to 3 do
                                Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
                            Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components
                            for i := 1 to 3 do  // Stuff it in the result
                            begin
                                arg[iV] := V012[i].re;
                                Inc(iV);
                                arg[iV] := V012[i].im;
                                Inc(iV);
                            end;
                        end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);
        end;
        11:
        begin  // Bus.VLL
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        jj := 1;
                        if Nvalues > 3 then
                            Nvalues := 3;
                        if Nvalues > 1 then
                        begin
                            if Nvalues = 2 then
                                Nvalues := 1;  // only one L-L voltage if 2 phase
                            arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                            iV := 0;
                            with pBus do
                            begin
                                if kVBase > 0.0 then
                                    BaseFactor := 1000.0 * kVBase * sqrt3
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
                                        NodeIdxi := FindIdx(jj);  // Get the index of the Node that matches i
                                        inc(jj);
                                    until NodeIdxi > 0;

                                    repeat
                                        NodeIdxj := FindIdx(jj);  // Get the index of the Node that matches i
                                        if jj > 3 then
                                            jj := 1
                                        else
                                            inc(jj);
                                    until NodeIdxj > 0;
//------------------------------------------------------------------------------------------------
//                      if jj>3 then jj := 1; // wrap around
//                      NodeIdxj := FindIdx(jj);
                                    with Solution do
                                        Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                                    arg[iV] := Volts.re;
                                    Inc(iV);
                                    arg[iV] := Volts.im;
                                    Inc(iV);
                                end;
                            end;  {With pBus}
                        end
                        else
                        begin  // for 1-phase buses, do not attempt to compute.
                            arg := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
                            arg[0] := -99999.0;
                            arg[1] := 0.0;
                        end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        12:
        begin   // Bus. PuVLL
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        if Nvalues > 3 then
                            Nvalues := 3;
                        if Nvalues > 1 then
                        begin
                            if Nvalues = 2 then
                                Nvalues := 1;  // only one L-L voltage if 2 phase
                            arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                            iV := 0;
                            with pBus do
                            begin
                                if kVBase > 0.0 then
                                    BaseFactor := 1000.0 * kVBase * sqrt3
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
                                        NodeIdxi := FindIdx(jj);  // Get the index of the Node that matches i
                                        inc(jj);
                                    until NodeIdxi > 0;

                                    repeat
                                        NodeIdxj := FindIdx(jj);  // Get the index of the Node that matches i
                                        if jj > 3 then
                                            jj := 1
                                        else
                                            inc(jj);
                                    until NodeIdxj > 0;
//------------------------------------------------------------------------------------------------
//                      if jj>3 then jj := 1; // wrap around
//                      NodeIdxj := FindIdx(jj);
                                    with Solution do
                                        Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                                    arg[iV] := Volts.re / BaseFactor;
                                    Inc(iV);
                                    arg[iV] := Volts.im / BaseFactor;
                                    Inc(iV);
                                end;
                            end;  {With pBus}
                        end
                        else
                        begin  // for 1-phase buses, do not attempt to compute.
                            arg := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
                            arg[0] := -99999.0;
                            arg[1] := 0.0;
                        end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        13:
        begin  // Bus.VMagAngle
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                        iV := 0;
                        jj := 1;
                        with pBus do
                            for i := 1 to NValues do
                            begin
                // this code so nodes come out in order from smallest to larges
                                repeat
                                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                                    inc(jj)
                                until NodeIdx > 0;
                                Voltsp := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                                arg[iV] := Voltsp.mag;
                                Inc(iV);
                                arg[iV] := Voltsp.ang;
                                Inc(iV);
                            end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        14:
        begin   // Bus.PuVMagAngle
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                        iV := 0;
                        jj := 1;
                        with pBus do
                        begin
                            if kVBase > 0.0 then
                                BaseFactor := 1000.0 * kVBase
                            else
                                BaseFactor := 1.0;
                            for i := 1 to NValues do
                            begin
                    // this code so nodes come out in order from smallest to larges
                                repeat
                                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                                    inc(jj)
                                until NodeIdx > 0;
                                Voltsp := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                                arg[iV] := Voltsp.mag / BaseFactor;
                                Inc(iV);
                                arg[iV] := Voltsp.ang;
                                Inc(iV);
                            end;
                        end;
                    end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        15:
        begin   // Bus.LineList
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    BusReference := ActiveBusIndex;
           { Count number of Lines connected to this bus }
                    LineCount := 0;
                    pElem := TDSSCktElement(Lines.First);
                    while Assigned(pElem) do
                    begin
                        if CheckBusReference(pElem, BusReference, j) then
                            Inc(LineCount);
                        pElem := TDSSCktElement(Lines.Next);
                    end;

                    if LineCount > 0 then
                    begin
           // Allocate Variant Array
                        arg := VarArrayCreate([0, LineCount - 1], varOleStr);
                        pElem := TDSSCktElement(Lines.First);
                        k := 0;
                        while Assigned(pElem) do
                        begin
                            if CheckBusReference(pElem, BusReference, j) then
                            begin
                                arg[k] := 'LINE.' + pElem.name;
                                Inc(k);
                            end;
                            pElem := TDSSCktElement(Lines.Next);
                        end;

                    end
                    else
                        arg := VarArrayCreate([0, 0], varOleStr);
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        16:
        begin   // Bus.LoadList
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                begin
                    BusReference := ActiveBusIndex;
         { Count number of LOAD elements connected to this bus }
                    LoadCount := 0;
                    pElem := TDSSCktElement(Loads.First);
                    while Assigned(pElem) do
                    begin
                        if CheckBusReference(pElem, BusReference, j) then
                            Inc(LoadCount);
                        pElem := TDSSCktElement(Loads.Next);
                    end;

                    if LoadCount > 0 then
                    begin
         // Allocate Variant Array
                        arg := VarArrayCreate([0, LoadCount - 1], varOleStr);

                        k := 0;
                        pElem := TDSSCktElement(Loads.First);
                        while Assigned(pElem) do
                        begin
                            if CheckBusReference(pElem, BusReference, j) then
                            begin
                                arg[k] := 'LOAD.' + pElem.name;
                                Inc(k);
                            end;
                            pElem := TDSSCktElement(Loads.Next);
                        end;

                    end
                    else
                        arg := VarArrayCreate([0, 0], varOleStr);
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        17:
        begin   // Bus.ZSC012Matrix
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble)
            end
            else
                with ActiveCircuit[ActiveActor] do
                    with ActiveCircuit[ActiveActor] do
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        with pBus do
                        begin

                            if NumNodesThisBus = 3 then
                            begin
                                Nvalues := SQR(NumNodesThisBus) * 2;  // Should be 9 complex numbers
                                arg := VarArrayCreate([0, NValues - 1], varDouble);
                      // Compute ZSC012 for 3-phase buses else leave it zeros
                      // ZSC012 = Ap2s Zsc As2p
                                Zsc012Temp := Zsc.MtrxMult(As2p);  // temp for intermediate result
                                if Assigned(ZSC012) then
                                    ZSC012.Free;
                                ZSC012 := Ap2s.MtrxMult(Zsc012Temp);
                      // Cleanup
                                Zsc012Temp.Free;

                  {Return all the elements of ZSC012}
                                k := 0;
                                pValues := pDoubleArray(ZSC012.GetValuesArrayPtr(Norder));
                                for i := 1 to Nvalues do
                                begin
                                    arg[k] := pValues^[i];
                                    Inc(k);
                                end;
                            end

                            else
                                arg := VarArrayCreate([0, 0], varDouble);   // default null array
                        end;
                    end;
        end;
        18:
        begin   // Bus.AllPCEatBus
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        myPXEList := getPCEatBus(BusList.Get(ActiveBusIndex));
                        arg := VarArrayCreate([0, length(myPXEList) - 1], varOleStr);
                        for i := 0 to High(myPXEList) do
                            arg[i] := myPXEList[i];
                    end
                    else
                        arg := VarArrayCreate([0, 0], varOleStr);
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end;
        19:
        begin   // Bus.AllPDEatBus
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        myPXEList := getPDEatBus(BusList.Get(ActiveBusIndex));
                        arg := VarArrayCreate([0, length(myPXEList) - 1], varOleStr);
                        for i := 0 to High(myPXEList) do
                            arg[i] := myPXEList[i];
                    end
                    else
                        arg := VarArrayCreate([0, 0], varOleStr);
                end
            else
                arg := VarArrayCreate([0, 0], varOleStr);
        end
    else
        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
    end;
end;

end.
