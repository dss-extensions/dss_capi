unit ImplBus;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TBus = class(TAutoObject, IBus)
    PROTECTED
        function Get_Name: Widestring; SAFECALL;
        function Get_NumNodes: Integer; SAFECALL;
        function Get_SeqVoltages: Olevariant; SAFECALL;
        function Get_Voltages: Olevariant; SAFECALL;
        function Get_Nodes: Olevariant; SAFECALL;
        function Get_Isc: Olevariant; SAFECALL;
        function Get_Voc: Olevariant; SAFECALL;
        function Get_kVBase: Double; SAFECALL;
        function Get_puVoltages: Olevariant; SAFECALL;
        function Get_Zsc0: Olevariant; SAFECALL;
        function Get_Zsc1: Olevariant; SAFECALL;
        function Get_ZscMatrix: Olevariant; SAFECALL;
        function ZscRefresh: Wordbool; SAFECALL;
        function Get_YscMatrix: Olevariant; SAFECALL;
        function Get_Coorddefined: Wordbool; SAFECALL;
        function Get_x: Double; SAFECALL;
        procedure Set_x(Value: Double); SAFECALL;
        function Get_y: Double; SAFECALL;
        procedure Set_y(Value: Double); SAFECALL;
        function Get_Distance: Double; SAFECALL;
        function GetUniqueNodeNumber(StartNumber: Integer): Integer; SAFECALL;
        function Get_CplxSeqVoltages: Olevariant; SAFECALL;
        function Get_Int_Duration: Double; SAFECALL;
        function Get_Lambda: Double; SAFECALL;
        function Get_Cust_Duration: Double; SAFECALL;
        function Get_Cust_Interrupts: Double; SAFECALL;
        function Get_N_Customers: Integer; SAFECALL;
        function Get_N_interrupts: Double; SAFECALL;
        function Get_puVLL: Olevariant; SAFECALL;
        function Get_VLL: Olevariant; SAFECALL;
        function Get_puVmagAngle: Olevariant; SAFECALL;
        function Get_VMagAngle: Olevariant; SAFECALL;
        function Get_TotalMiles: Double; SAFECALL;
        function Get_SectionID: Integer; SAFECALL;
        function Get_LineList: Olevariant; SAFECALL;
        function Get_LoadList: Olevariant; SAFECALL;
        function Get_ZSC012Matrix: Olevariant; SAFECALL;
        function Get_Latitude: Double; SAFECALL;
        procedure Set_Latitude(Value: Double); SAFECALL;
        function Get_Longitude: Double; SAFECALL;
        procedure Set_Longitude(Value: Double); SAFECALL;
        function Get_AllPCEatBus: Olevariant; SAFECALL;
        function Get_AllPDEatBus: Olevariant; SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    ImplGlobals,
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

function TBus.Get_Name: Widestring;
begin
    Result := '';

    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                Result := BusList.Get(ActiveBusIndex);
end;

function TBus.Get_NumNodes: Integer;
begin
    Result := 0;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].NumNodesThisBus;

end;

function TBus.Get_SeqVoltages: Olevariant;

// Compute sequence voltages for Active Bus
// magnitude only
// returns a set of seq voltages (3)

var
    Nvalues, i, iV: Integer;
    VPh, V012: array[1..3] of Complex;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
                if Nvalues > 3 then
                    Nvalues := 3;

      // Assume nodes 1, 2, and 3 are the 3 phases
                Result := VarArrayCreate([0, 2], varDouble);
                if Nvalues <> 3 then
                    for i := 1 to 3 do
                        Result[i - 1] := -1.0  // Signify seq voltages n/A for less then 3 phases
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
                        Result[iV] := Cabs(V012[i]);
                        Inc(iV);
                    end;

                end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);


end;

function TBus.Get_Voltages: Olevariant;
// Return Complex for all nodes of voltages for Active Bus

var
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: Complex;
    pBus: TDSSBus;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                pBus := Buses^[ActiveBusIndex];
                Nvalues := pBus.NumNodesThisBus;
                Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
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
                        Result[iV] := Volts.re;
                        Inc(iV);
                        Result[iV] := Volts.im;
                        Inc(iV);
                    end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_Nodes: Olevariant;

// return array of node numbers corresponding to voltages

var
    Nvalues, i, iV, NodeIdx, jj: Integer;
    pBus: TDSSBus;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varInteger)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                pBus := Buses^[ActiveBusIndex];
                with pBus do
                begin
                    Nvalues := NumNodesThisBus;
                    Result := VarArrayCreate([0, NValues - 1], varInteger);
                    iV := 0;
                    jj := 1;
                    for i := 1 to NValues do
                    begin
                // this code so nodes come out in order from smallest to larges
                        repeat
                            NodeIdx := FindIdx(jj);  // Get the index of the Node that matches jj
                            inc(jj)
                        until NodeIdx > 0;
                        Result[iV] := Buses^[ActiveBusIndex].GetNum(NodeIdx);
                        Inc(iV);
                    end;
                end;
            end
            else
                Result := VarArrayCreate([0, 0], varInteger);  // just return null array

end;

function TBus.Get_Isc: Olevariant;

// Return the short circuit current

var
    Isc: Complex;
    i, iV, NValues: Integer;

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                if Buses^[ActiveBusIndex].BusCurrent <> NIL then
                begin
                    NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
                    Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                    iV := 0;
                    for i := 1 to NValues do
                    begin
                        Isc := Buses^[ActiveBusIndex].BusCurrent^[i];
                        Result[iV] := Isc.Re;
                        Inc(iV);
                        Result[iV] := Isc.Im;
                        Inc(iV);
                    end;
                end
                else
                    Result := VarArrayCreate([0, 0], varDouble);
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_Voc: Olevariant;

// Return the Open circuit Voltage for this bus

var
    Voc: Complex;
    i, iV, NValues: Integer;

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                if Buses^[ActiveBusIndex].VBus <> NIL then
                begin
                    NValues := Buses^[ActiveBusIndex].NumNodesThisBus;
                    Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                    iV := 0;
                    for i := 1 to NValues do
                    begin
                        Voc := Buses^[ActiveBusIndex].VBus^[i];
                        Result[iV] := Voc.Re;
                        Inc(iV);
                        Result[iV] := Voc.Im;
                        Inc(iV);
                    end;
                end
                else
                    Result := VarArrayCreate([0, 0], varDouble);
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array


end;

function TBus.Get_kVBase: Double;
begin
    Result := 0.0;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                Result := ActiveCircuit[ActiveActor].Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].kVBase;

end;

function TBus.Get_puVoltages: Olevariant;

// Returns voltages at bus in per unit.  However, if kVBase=0, returns actual volts

var
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: Complex;
    BaseFactor: Double;
    pBus: TDSSBus;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                pBus := Buses^[ActiveBusIndex];
                with pBus do
                begin
                    Nvalues := NumNodesThisBus;
                    Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
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
                        Result[iV] := Volts.re / BaseFactor;
                        Inc(iV);
                        Result[iV] := Volts.im / BaseFactor;
                        Inc(iV);
                    end;
                end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_Zsc0: Olevariant;

var
    Z: Complex;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Z := Buses^[ActiveBusIndex].Zsc0;
                Result := VarArrayCreate([0, 1], varDouble);
                Result[0] := Z.Re;
                Result[1] := Z.Im;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array
end;

function TBus.Get_Zsc1: Olevariant;

var
    Z: Complex;

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Z := Buses^[ActiveBusIndex].Zsc1;
                Result := VarArrayCreate([0, 1], varDouble);
                Result[0] := Z.Re;
                Result[1] := Z.Im;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_ZscMatrix: Olevariant;

var
    Nelements, iV, i, j: Integer;
    Z: Complex;

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else

        try

            with ActiveCircuit[ActiveActor] do
                if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                begin
                    if Assigned(Buses^[ActiveBusIndex].Zsc) then
                    begin
                        Nelements := Buses^[ActiveBusIndex].Zsc.Order;
                        Result := VarArrayCreate([0, ((2 * Nelements * Nelements) - 1)], varDouble);
                        iV := 0;
                        with Buses^[ActiveBusIndex] do
                            for i := 1 to Nelements do
                                for j := 1 to Nelements do
                                begin
                                    Z := Zsc.GetElement(i, j);
                                    Result[iV] := Z.Re;
                                    Inc(iV);
                                    Result[iV] := Z.Im;
                                    Inc(iV);
                                end;

                    end
                    else
                        Result := VarArrayCreate([0, 0], varDouble);  // just return null array
                end
                else
                    Result := VarArrayCreate([0, 0], varDouble);  // just return null array

        except
            On E: Exception do
                DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5016);
        end;

end;

function TBus.ZscRefresh: Wordbool;


begin

    Result := FALSE;   // Init in case of failure

    if ExecHelper.DoZscRefresh(ActiveActor) = 0 then
        Result := TRUE;

end;

function TBus.Get_YscMatrix: Olevariant;

var
    Nelements, iV, i, j: Integer;
    Y1: Complex;

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else

        try

            with ActiveCircuit[ActiveActor] do
                if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                begin
                    if Assigned(Buses^[ActiveBusIndex].Ysc) then
                    begin
                        Nelements := Buses^[ActiveBusIndex].Ysc.Order;
                        Result := VarArrayCreate([0, ((2 * Nelements * Nelements) - 1)], varDouble);
                        iV := 0;
                        with Buses^[ActiveBusIndex] do
                            for i := 1 to Nelements do
                                for j := 1 to Nelements do
                                begin
                                    Y1 := Ysc.GetElement(i, j);
                                    Result[iV] := Y1.Re;
                                    Inc(iV);
                                    Result[iV] := Y1.Im;
                                    Inc(iV);
                                end;

                    end
                    else
                        Result := VarArrayCreate([0, 0], varDouble);  // just return null array
                end
                else
                    Result := VarArrayCreate([0, 0], varDouble);  // just return null array

        except
            On E: Exception do
                DoSimpleMsg('ZscMatrix Error: ' + E.message + CRLF, 5017);
        end;


end;

function TBus.Get_Coorddefined: Wordbool;
begin
    Result := FALSE;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                    Result := TRUE;
end;

function TBus.Get_x: Double;
begin
    Result := 0.0;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                    Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x;
end;

procedure TBus.Set_x(Value: Double);
begin
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
                Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].x := Value;
            end;
end;

function TBus.Get_y: Double;
begin
    Result := 0.0;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                    Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].y;
end;

procedure TBus.Set_y(Value: Double);
begin
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Buses^[ActiveBusIndex].Coorddefined := TRUE;
                Buses^[ActiveBusIndex].y := Value;
            end;
end;

function TBus.Get_Distance: Double;
begin
    Result := 0.0;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if ((ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses)) then
                Result := Buses^[ActiveBusIndex].DistFromMeter;
end;

function TBus.GetUniqueNodeNumber(StartNumber: Integer): Integer;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do

            if ActiveBusIndex > 0 then
                Result := Utilities.GetUniqueNodeNumber(BusList.Get(ActiveBusIndex), StartNumber);
end;

function TBus.Get_CplxSeqVoltages: Olevariant;

// Compute sequence voltages for Active Bus
// Complex values
// returns a set of seq voltages (3) in 0, 1, 2 order

var
    Nvalues, i, iV: Integer;
    VPh, V012: array[1..3] of Complex;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Nvalues := Buses^[ActiveBusIndex].NumNodesThisBus;
                if Nvalues > 3 then
                    Nvalues := 3;

      // Assume nodes labelled 1, 2, and 3 are the 3 phases
                Result := VarArrayCreate([0, 5], varDouble);
                if Nvalues <> 3 then
                    for i := 1 to 6 do
                        Result[i - 1] := -1.0  // Signify seq voltages n/A for less then 3 phases
                else
                begin
                    iV := 0;
                    for i := 1 to 3 do
                        Vph[i] := Solution.NodeV^[Buses^[ActiveBusIndex].Find(i)];

                    Phase2SymComp(@Vph, @V012);   // Compute Symmetrical components

                    for i := 1 to 3 do  // Stuff it in the result
                    begin
                        Result[iV] := V012[i].re;
                        Inc(iV);
                        Result[iV] := V012[i].im;
                        Inc(iV);
                    end;
                end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);

end;

function TBus.Get_Int_Duration: Double;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].Bus_Int_Duration;
end;

function TBus.Get_Lambda: Double;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].BusFltRate;
end;

function TBus.Get_Cust_Duration: Double;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].BusCustDurations;
end;

function TBus.Get_Cust_Interrupts: Double;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].BusCustDurations;
end;

function TBus.Get_N_Customers: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].BusTotalNumCustomers;
end;

function TBus.Get_N_interrupts: Double;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].Bus_Num_Interrupt;
end;


function TBus.Get_puVLL: Olevariant;

var
    Nvalues, i, iV, NodeIdxi, NodeIdxj, jj: Integer;
    Volts: Complex;
    pBus: TDSSBus;
    BaseFactor: Double;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
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
                    Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
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
//                  if jj>3 then jj := 1; // wrap around
//                  NodeIdxj := FindIdx(jj);

                            with Solution do
                                Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                            Result[iV] := Volts.re / BaseFactor;
                            Inc(iV);
                            Result[iV] := Volts.im / BaseFactor;
                            Inc(iV);
                        end;
                    end;  {With pBus}
                end
                else
                begin  // for 1-phase buses, do not attempt to compute.
                    Result := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
                    Result[0] := -99999.0;
                    Result[1] := 0.0;
                end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array


end;

function TBus.Get_VLL: Olevariant;
var
    Nvalues, i, iV, NodeIdxi, NodeIdxj, jj: Integer;
    Volts: Complex;
    pBus: TDSSBus;

begin
    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
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
                    Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
                    iV := 0;
                    with pBus do
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
//                  if jj>3 then jj := 1; // wrap around
//                  NodeIdxj := FindIdx(jj);

                            with Solution do
                                Volts := Csub(NodeV^[GetRef(NodeIdxi)], NodeV^[GetRef(NodeIdxj)]);
                            Result[iV] := Volts.re;
                            Inc(iV);
                            Result[iV] := Volts.im;
                            Inc(iV);
                        end;
                end
                else
                begin  // for 1-phase buses, do not attempt to compute.
                    Result := VarArrayCreate([0, 1], varDouble);  // just return -1's in array
                    Result[0] := -99999.0;
                    Result[1] := 0.0;
                end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_puVmagAngle: Olevariant;
// Return mag/angle for all nodes of voltages for Active Bus

var
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: polar;
    pBus: TDSSBus;
    Basefactor: Double;

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                pBus := Buses^[ActiveBusIndex];
                Nvalues := pBus.NumNodesThisBus;
                Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
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

                        Volts := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                        Result[iV] := Volts.mag / BaseFactor;
                        Inc(iV);
                        Result[iV] := Volts.ang;
                        Inc(iV);
                    end;
                end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_VMagAngle: Olevariant;
// Return mag/angle for all nodes of voltages for Active Bus

var
    Nvalues, i, iV, NodeIdx, jj: Integer;
    Volts: polar;
    pBus: TDSSBus;

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
    end
    else
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                pBus := Buses^[ActiveBusIndex];
                Nvalues := pBus.NumNodesThisBus;
                Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);
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

                        Volts := ctopolardeg(Solution.NodeV^[GetRef(NodeIdx)]);  // referenced to pBus
                        Result[iV] := Volts.mag;
                        Inc(iV);
                        Result[iV] := Volts.ang;
                        Inc(iV);
                    end;
            end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

function TBus.Get_TotalMiles: Double;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].BusTotalMiles;
end;

function TBus.Get_SectionID: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if ActiveBusIndex > 0 then
                Result := Buses^[ActiveBusIndex].BusSectionID;
end;

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


function TBus.Get_LineList: Olevariant;
 { Returns list of LINE elements connected to this bus}
var
    BusReference, i, j, k, LineCount: Integer;
    pElem: TDSSCktElement;

begin

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
                Result := VarArrayCreate([0, LineCount - 1], varOleStr);
                pElem := TDSSCktElement(Lines.First);
                k := 0;
                while Assigned(pElem) do
                begin
                    if CheckBusReference(pElem, BusReference, j) then
                    begin
                        Result[k] := 'LINE.' + pElem.name;
                        Inc(k);
                    end;
                    pElem := TDSSCktElement(Lines.Next);
                end;

            end
            else
                Result := VarArrayCreate([0, 0], varOleStr);
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

function TBus.Get_LoadList: Olevariant;

{ Returns list of LOAD elements connected to this bus}

var
    BusReference, i, j, k, LoadCount: Integer;
    pElem: TDSSCktElement;

begin

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
                Result := VarArrayCreate([0, LoadCount - 1], varOleStr);

                k := 0;
                pElem := TDSSCktElement(Loads.First);
                while Assigned(pElem) do
                begin
                    if CheckBusReference(pElem, BusReference, j) then
                    begin
                        Result[k] := 'LOAD.' + pElem.name;
                        Inc(k);
                    end;
                    pElem := TDSSCktElement(Loads.Next);
                end;

            end
            else
                Result := VarArrayCreate([0, 0], varOleStr);
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

function TBus.Get_ZSC012Matrix: Olevariant;

var
    Zsc012Temp: TCmatrix;
    NValues: Integer;
    Norder: Integer;
    i, k: Integer;
    pBus: TDSSBus;
    pValues: pDoubleArray;   // Temp array to move the data

begin

    if ActiveCircuit[ActiveActor] = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble)
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
                        Result := VarArrayCreate([0, NValues - 1], varDouble);
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
                            Result[k] := pValues^[i];
                            Inc(k);
                        end;
                    end

                    else
                        Result := VarArrayCreate([0, 0], varDouble);   // default null array
                end;
            end;

end;

function TBus.Get_Latitude: Double;
begin
    Result := 0.0;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                    Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat;
end;


procedure TBus.Set_Latitude(Value: Double);
begin
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
                Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].lat := Value;
            end;
end;


function TBus.Get_Longitude: Double;
begin
    Result := 0.0;
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
                if (Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined) then
                    Result := Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long;
end;

procedure TBus.Set_Longitude(Value: Double);
begin
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].Coorddefined := TRUE;
                Buses^[ActiveCircuit[ActiveActor].ActiveBusIndex].long := Value;
            end;
end;

function TBus.Get_AllPCEatBus: Olevariant;
var
    i: Integer;
    myPCEList: DynStringArray;

begin
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
        begin
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                myPCEList := getPCEatBus(BusList.Get(ActiveBusIndex));
                Result := VarArrayCreate([0, length(myPCEList) - 1], varOleStr);
                for i := 0 to High(myPCEList) do
                    Result[i] := myPCEList[i];
            end
            else
                Result := VarArrayCreate([0, 0], varOleStr);
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);
end;

function TBus.Get_AllPDEatBus: Olevariant;
var
    i: Integer;
    myPDEList: DynStringArray;

begin
    if (ActiveCircuit[ActiveActor] <> NIL) then
        with ActiveCircuit[ActiveActor] do
        begin
            if (ActiveBusIndex > 0) and (ActiveBusIndex <= Numbuses) then
            begin
                myPDEList := getPDEatBus(BusList.Get(ActiveBusIndex));
                Result := VarArrayCreate([0, length(myPDEList) - 1], varOleStr);
                for i := 0 to High(myPDEList) do
                    Result[i] := myPDEList[i];
            end
            else
                Result := VarArrayCreate([0, 0], varOleStr);
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);

end;

initialization
    TAutoObjectFactory.Create(ComServer, TBus, Class_Bus, ciInternal, tmApartment);
end.
