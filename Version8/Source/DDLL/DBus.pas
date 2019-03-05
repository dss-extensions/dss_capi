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
    Bus;

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
    Nvalues, i, iV, NodeIdx, jj, NodeIdxj, NodeIdxi: Integer;
    Volts, Voc, Isc: Complex;
    pBus: TDSSBus;
    VPh, V012: array[1..3] of Complex;
    BaseFactor: Double;
    Nelements, j: Integer;
    Z, Y1: Complex;
    voltsp: polar;

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
                                    NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                                    jj := i + 1;
                                    if jj > 3 then
                                        jj := 1; // wrap around
                                    NodeIdxj := FindIdx(jj);
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
                                    NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                                    jj := i + 1;
                                    if jj > 3 then
                                        jj := 1; // wrap around
                                    NodeIdxj := FindIdx(jj);
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
        end
    else
        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
    end;
end;

end.
