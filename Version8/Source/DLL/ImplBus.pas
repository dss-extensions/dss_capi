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
    Bus;

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
                            NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                            jj := i + 1;
                            if jj > 3 then
                                jj := 1; // wrap around
                            NodeIdxj := FindIdx(jj);

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
                  // this code so nodes come out in order from smallest to largest
                            NodeIdxi := FindIdx(i);  // Get the index of the Node that matches i
                            jj := i + 1;
                            if jj > 3 then
                                jj := 1; // wrap around
                            NodeIdxj := FindIdx(jj);

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

initialization
    TAutoObjectFactory.Create(ComServer, TBus, Class_Bus, ciInternal, tmApartment);
end.
