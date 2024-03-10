unit DBus;

interface

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

function BUSI(mode: Longint; arg: Longint): Longint; CDECL;
function BUSF(mode: Longint; arg: Double): Double; CDECL;
function BUSS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure BUSV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

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
function BUSS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
begin
    Result := Pansichar(Ansistring('0')); //Default return value
    case mode of
        0:
        begin                                           // Bus.Name read
            if (ActiveCircuit[ActiveActor] <> NIL) then
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        Result := Pansichar(Ansistring(BusList.Get(ActiveBusIndex)));
        end
    else
        Result := Pansichar(Ansistring('Error, Parameter non recognized'));
    end;
end;

procedure BUSV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

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
    Volts: Complex;
    pBus: TDSSBus;
    VPh,
    V012: array[1..3] of Complex;
    BaseFactor: Double;
    voltsp: polar;
    pElem: TDSSCktElement;
    Zsc012Temp: TCmatrix;
    pValues: pDoubleArray;
    myPXEList: DynStringArray;
    S: String;

begin
    case mode of
        0:
        begin                                           // Bus.Voltages
            myType := 3;                  // Complex
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                setlength(myCmplxArray, 1);
                myCmplxArray[0] := CZero;
            end
            else
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        setlength(myCmplxArray, 0);
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        jj := 1;
                        with pBus do
                            for i := 1 to NValues do
                            begin
          // this code so nodes come out in order from smallest to larges
                                repeat
                                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                                    inc(jj)
                                until NodeIdx > 0;
                                setlength(myCmplxArray, length(myCmplxArray) + 1);
                                myCmplxArray[High(myCmplxArray)] := Solution.NodeV^[GetRef(NodeIdx)];   // referenced to pBus
                            end;
                    end

                end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * length(myCmplxArray);
        end;

        1:
        begin                                           // Bus.SeqVoltages
            myType := 3;
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                setlength(myDBLArray, 1);
                myDBLArray[0] := 0;
            end
            else
                with ActiveCircuit[ActiveActor] do
                begin
                    setlength(myDBLArray, 3);
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
                        if Nvalues <> 3 then
                            for i := 1 to 3 do
                                myDBLArray[i - 1] := -1.0  // Signify seq voltages n/A for less then 3 phases
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
                                myDBLArray[iV] := Cabs(V012[i]);
                                Inc(iV);
                            end;
                        end;
                    end;
                end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * 3;
        end;
        2:
        begin                                           // Bus.Nodes
            myType := 1;
            if ActiveCircuit[ActiveActor] = NIL then
            begin
                setlength(myIntArray, 1);
                myIntArray[0] := 0;
            end
            else
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        with pBus do
                        begin
                            setlength(myIntArray, NumNodesThisBus);
                            Nvalues := NumNodesThisBus;
                            iV := 0;
                            jj := 1;
                            for i := 1 to NValues do
                            begin
              // this code so nodes come out in order from smallest to larges
                                repeat
                                    NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                                    inc(jj)
                                until NodeIdx > 0;
                                myIntArray[iV] := Buses^[ActiveBusIndex].GetNum(NodeIdx);
                                Inc(iV);
                            end;
                        end;
                    end;
            myPointer := @(myIntArray[0]);
            mySize := SizeOf(myIntArray[0]) * Length(myIntArray);
        end;
        3:
        begin                                           // Bus.Voc
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        if Buses^[ActiveBusIndex].VBus <> NIL then
                        begin
                            NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
                            setlength(myCmplxArray, NValues);
                            for i := 1 to NValues do
                            begin
                                myCmplxArray[i - 1] := Buses^[ActiveBusIndex].VBus^[i];
                            end;
                        end;
                    end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        4:
        begin // Bus.Isc
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        if Buses^[ActiveBusIndex].BusCurrent <> NIL then
                        begin
                            NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
                            setlength(myCmplxArray, NValues);
                            for i := 1 to NValues do
                            begin
                                myCmplxArray[i - 1] := Buses^[ActiveBusIndex].BusCurrent^[i];
                            end;
                        end;
                    end;
                end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        5:
        begin  // Bus.PuVoltages
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        with pBus do
                        begin
                            Nvalues := NumNodesThisBus;
                            setlength(myCmplxArray, NValues);
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
                                myCmplxArray[i - 1] := cdivreal(Solution.NodeV^[GetRef(NodeIdx)], BaseFactor);
                            end;
                        end;
                    end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        6:
        begin  // Bus.ZscMatrix
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                try
                    with ActiveCircuit[ActiveActor] do
                        if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        begin
                            if Assigned(Buses^[ActiveBusIndex].Zsc) then
                            begin
                                Nelements := Buses^[ActiveBusIndex].Zsc.Order;
                                setlength(myCmplxArray, (Nelements * Nelements));
                                iV := 0;
                                with Buses^[ActiveBusIndex] do
                                    for i := 1 to Nelements do
                                        for j := 1 to Nelements do
                                        begin
                                            myCmplxArray[iV] := Zsc.GetElement(i, j);
                                            inc(iV);
                                        end;
                            end;
                        end;
                except
                    On E: Exception do
                        DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5016);
                end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        7:
        begin  // Bus.Zcs1
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        myCmplxArray[0] := Buses^[ActiveBusIndex].Zsc1;
                    end
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        8:
        begin  // Bus.Zsc0
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        myCmplxArray[0] := Buses^[ActiveBusIndex].Zsc0;
                    end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        9:
        begin   // Bus.YscMatrix
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                try
                    with ActiveCircuit[ActiveActor] do
                        if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                        begin
                            if Assigned(Buses^[ActiveBusIndex].Ysc) then
                            begin
                                Nelements := Buses^[ActiveBusIndex].Ysc.Order;
                                setlength(myCmplxArray, (Nelements * Nelements));
                                iV := 0;
                                with Buses^[ActiveBusIndex] do
                                    for i := 1 to Nelements do
                                        for j := 1 to Nelements do
                                        begin
                                            myCmplxArray[iV] := Ysc.GetElement(i, j);
                                            Inc(iV);
                                        end;
                            end;
                        end;
                except
                    On E: Exception do
                        DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5017);
                end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        10:
        begin  // Bus.CplxSeqVoltages
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
                        setlength(myCmplxArray, 3);
                        if Nvalues <> 3 then
                            for i := 0 to 2 do
                                myCmplxArray[i] := cmplx(-1.0, -1.0)  // Signify seq voltages n/A for less then 3 phases
                        else
                        begin
                            iV := 0;
                            for i := 1 to 3 do
                                Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];
                            Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components
                            for i := 1 to 3 do  // Stuff it in the result
                            begin
                                myCmplxArray[iV] := V012[i];
                                Inc(iV);
                            end;
                        end;
                    end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        11:
        begin  // Bus.VLL
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
                            setlength(myCmplxArray, NValues);
                            iV := 0;
                            with pBus do
                            begin
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
                                    myCmplxArray[iV] := Volts;
                                    Inc(iV);
                                end;
                            end;  {With pBus}
                        end
                        else
                        begin  // for 1-phase buses, do not attempt to compute.
                            setlength(myCmplxArray, 1);  // just return -1's in array
                            myCmplxArray[0] := cmplx(-99999.0, 0);
                        end;
                    end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        12:
        begin   // Bus. PuVLL
            myType := 3;
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
                            setlength(myCmplxArray, NValues);
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
                                    myCmplxArray[iV] := cdivreal(Volts, BaseFactor);
                                    Inc(iV);
                                end;
                            end;  {With pBus}
                        end
                        else
                        begin  // for 1-phase buses, do not attempt to compute.
                            setlength(myCmplxArray, 1);  // just return -1's in array
                            myCmplxArray[0] := cmplx(-99999.0, 0);
                        end;
                    end;
            end;
            myPointer := @(myCmplxArray[0]);
            mySize := SizeOf(myCmplxArray[0]) * Length(myCmplxArray);
        end;
        13:
        begin  // Bus.VMagAngle
            myType := 3;
            setlength(myPolarArray, 1);
            myPolarArray[0] := ctopolar(CZero);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        setlength(myPolarArray, NValues);
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
                                myPolarArray[iV] := Voltsp;
                                Inc(iV);
                            end;
                    end
            end;
            myPointer := @(myPolarArray[0]);
            mySize := SizeOf(myPolarArray[0]) * Length(myPolarArray);
        end;
        14:
        begin   // Bus.PuVMagAngle
            myType := 3;
            setlength(myPolarArray, 1);
            myPolarArray[0] := ctopolar(CZero);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        pBus := Buses^[ActiveBusIndex];
                        Nvalues := pBus.NumNodesThisBus;
                        setlength(myPolarArray, NValues);
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
                                myPolarArray[iV] := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                                myPolarArray[iV].mag := myPolarArray[iV].mag / BaseFactor;
                                Inc(iV);
                            end;
                        end;
                    end;
            end;
            myPointer := @(myPolarArray[0]);
            mySize := SizeOf(myPolarArray[0]) * Length(myPolarArray);
        end;
        15:
        begin   // Bus.LineList
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
       // Allocate Array
                        setlength(myStrArray, 0);
                        pElem := TDSSCktElement(Lines.First);
                        while Assigned(pElem) do
                        begin
                            if CheckBusReference(pElem, BusReference, j) then
                            begin
                                S := 'LINE.' + pElem.name;
                                WriteStr2Array(S);
                                WriteStr2Array(Char(0));
                            end;
                            pElem := TDSSCktElement(Lines.Next);
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        16:
        begin   // Bus.LoadList
            myType := 4;        // String
            setlength(myStrArray, 0);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
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
       // Allocate Array
                        setlength(myStrArray, 0);
                        pElem := TDSSCktElement(Loads.First);
                        while Assigned(pElem) do
                        begin
                            if CheckBusReference(pElem, BusReference, j) then
                            begin
                                S := 'LOAD.' + pElem.name;
                                WriteStr2Array(S);
                                WriteStr2Array(Char(0));
                            end;
                            pElem := TDSSCktElement(Loads.Next);
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        17:
        begin   // Bus.ZSC012Matrix
            myType := 2;          // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    pBus := Buses^[ActiveBusIndex];
                    with pBus do
                    begin
                        if NumNodesThisBus = 3 then
                        begin
                            Nvalues := SQR(NumNodesThisBus) * 2;  // Should be 9 complex numbers
                            setlength(myDBLArray, NValues);
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
                                myDBLArray[k] := pValues^[i];
                                Inc(k);
                            end;
                        end
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        18:
        begin   // Bus.AllPCEatBus
            myType := 4;        // String
            setlength(myStrArray, 0);
            if (ActiveCircuit[ActiveActor] <> NIL) then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        myPXEList := getPCEatBus(BusList.Get(ActiveBusIndex));
                        setlength(myStrArray, 0);
                        for i := 0 to High(myPXEList) do
                        begin
                            if myPXEList[i] <> '' then
                            begin
                                WriteStr2Array(myPXEList[i]);
                                WriteStr2Array(Char(0));
                            end;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        19:
        begin   // Bus.AllPDEatBus
            myType := 4;        // String
            setlength(myStrArray, 0);
            if (ActiveCircuit[ActiveActor] <> NIL) then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                    begin
                        myPXEList := getPDEatBus(BusList.Get(ActiveBusIndex));
                        setlength(myStrArray, 0);
                        for i := 0 to High(myPXEList) do
                        begin
                            if myPXEList[i] <> '' then
                            begin
                                for j := 1 to High(myPXEList[i]) do
                                begin
                                    setlength(myStrArray, length(myStrArray) + 1);
                                    myStrArray[High(myStrArray)] := Byte(myPXEList[i][j]);
                                end;
                                setlength(myStrArray, length(myStrArray) + 1);
                                myStrArray[High(myStrArray)] := Byte(0);
                            end;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
    else
        myType := 4;        // String
        WriteStr2Array('Command not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
end;

end.
