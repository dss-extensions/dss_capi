unit ImplDSSimComs;

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
    OpenDSSengine_TLB,
    StdVcl,
    UComplex;

type
    TDSSimComs = class(TAutoObject, IDSSimComs)

    PROTECTED
//    function Get_I0: OleVariant; safecall;
        function BusVoltagepu(Index: SYSUINT): Olevariant; SAFECALL;
        function BusVoltage(Index: SYSUINT): Olevariant; SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    Dialogs,
    SysUtils,
    solution,
    Variants,
    CktElement;

{*    // This routine is under test, the aim is to get the actual inj currents vector
function TDSSimComs.Get_I0: OleVariant;
var
NNodes   : Integer;
Buses :Integer;
I     : Integer;
begin
    NNodes:=SQR(ActiveCircuit.NumNodes);
    Buses := ActiveCircuit.NumNodes;
    Result := VarArrayCreate( [0, 2*NNodes -1], varDouble);
    for I := 0 to 2*NNodes - 1 do Result[I] := 0.0;
    for I := 0 to Buses do
      begin
          Result[I*2] := Solution.ActiveSolutionObj.I0[I+1].re;
          Result[I*2+1] := Solution.ActiveSolutionObj.I0[I+1].im;
      end;
end;
*}
function TDSSimComs.BusVoltagepu(Index: SYSUINT): Olevariant;
var
    i, j: Integer;
    Volts, BaseFactor: Double;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            i := Index;
            Result := VarArrayCreate([0, Buses^[i].NumNodesThisBus - 1], varDouble);
            if Buses^[i].kVBase > 0.0 then
                BaseFactor := 1000.0 * Buses^[i].kVBase
            else
                BaseFactor := 1.0;
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Volts := Cabs(ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)]);
                Result[j - 1] := Volts / BaseFactor;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);
end;

function TDSSimComs.BusVoltage(Index: SYSUINT): Olevariant;
var
    i, j, k: Integer;
    Volts: Complex;
begin
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            i := Index;
            Result := VarArrayCreate([0, 2 * Buses^[i].NumNodesThisBus - 1], varDouble);
            for j := 1 to Buses^[i].NumNodesThisBus do
            begin
                Volts := ActiveCircuit.Solution.NodeV^[Buses^[i].GetRef(j)];
                k := (j - 1) * 2;
                Result[k] := Volts.re;
                Result[k + 1] := Volts.im;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varDouble);

end;

initialization
    TAutoObjectFactory.Create(ComServer, TDSSimComs, Class_DSSimComs,
        ciInternal, tmApartment);
end.
