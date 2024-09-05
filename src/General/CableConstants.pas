unit CableConstants;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------
interface

uses
    Arraydef,
    Ucmatrix,
    UComplex, DSSUcomplex,
    LineUnits,
    LineConstants;

type

    TCableConstants = class(TLineConstants)
    PRIVATE
    PROTECTED
        FEpsR: pDoubleArray;
        FInsLayer: pDoubleArray;
        FDiaIns: pDoubleArray;
        FDiaCable: pDoubleArray;

    PUBLIC
        function ConductorsInSameSpace(var ErrorMessage: String): Boolean; OVERRIDE;
        procedure Kron(Norder: Integer); OVERRIDE; // don't reduce Y, it has zero neutral capacitance

        constructor Create(NConductors: Integer);
        destructor Destroy; OVERRIDE;

        procedure SetEpsR(i: Integer; const Value: Double);
        procedure SetInsLayer(i, units: Integer; const Value: Double);
        procedure SetDiaIns(i, units: Integer; const Value: Double);
        procedure SetDiaCable(i, units: Integer; const Value: Double);
    end;

implementation

uses
    SysUtils;

procedure TCableConstants.Kron(Norder: Integer);
var
    Ztemp: TCmatrix;
    FirstTime: Boolean;
    i, j: Integer;
begin
    Ztemp := FZMatrix;
    FirstTime := TRUE;
    if (FFrequency >= 0.0) and (Norder > 0) and (Norder < numConductors) then
    begin
        if Assigned(FZreduced) then
            FZreduced.Free;
        if Assigned(FYCreduced) then
            FYCReduced.Free;
        while Ztemp.order > Norder do
        begin
            FZReduced := Ztemp.Kron(ZTemp.order);    // Eliminate last row
            if not FirstTime then
                Ztemp.Free;  // Ztemp points to intermediate matrix
            Ztemp := FZReduced;
            FirstTime := FALSE;
        end;
        // now copy part of FYCmatrix to FYCreduced
        FYCreduced := TCmatrix.CreateMatrix(Norder);
        for i := 1 to Norder do
            for j := 1 to Norder do
                FYCreduced[i, j] := FYCmatrix[i, j];
    end;
end;

function TCableConstants.ConductorsInSameSpace(var ErrorMessage: String): Boolean;
var
    i, j: Integer;
    Dij: Double;
    Ri, Rj: Double;
begin
    Result := FALSE;

//  Height of cable doesn't matter
//  Removed 5-25-2016 RcD
//  For i := 1 to numConductors do Begin
//    if (FY[i] >= 0.0) then Begin
//      Result := TRUE;
//      ErrorMessage :=
//        Format('Cable %d height must be < 0. ', [ i ]);
//      Exit
//    End;
//  End;
    for i := 1 to numConductors do
    begin
        if i <= FNPhases then
            Ri := FRadius[i]
        else
            Ri := 0.5 * FDiaCable[i];
        for j := i + 1 to numConductors do
        begin
            if j <= FNPhases then
                Rj := FRadius[j]
            else
                Rj := 0.5 * FDiaCable[j];
            Dij := Sqrt(SQR(FX[i] - FX[j]) + SQR(FY[i] - FY[j]));
            if (Dij < (Ri + Rj)) then
            begin
                Result := TRUE;
                ErrorMessage := Format('Cable conductors %d and %d occupy the same space.', [i, j]);
                Exit;
            end;
        end;
    end;
end;

procedure TCableConstants.SetEpsR(i: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FEpsR[i] := Value;
end;

procedure TCableConstants.SetInsLayer(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FInsLayer[i] := Value * To_Meters(units);
end;

procedure TCableConstants.SetDiaIns(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FDiaIns[i] := Value * To_Meters(units);
end;

procedure TCableConstants.SetDiaCable(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FDiaCable[i] := Value * To_Meters(units);
end;

constructor TCableConstants.Create(NConductors: Integer);
begin
    inherited Create(NConductors);
    FEpsR := Allocmem(Sizeof(FEpsR[1]) * numConductors);
    FInsLayer := Allocmem(Sizeof(FInsLayer[1]) * numConductors);
    FDiaIns := Allocmem(Sizeof(FDiaIns[1]) * numConductors);
    FDiaCable := Allocmem(Sizeof(FDiaCable[1]) * numConductors);
end;

destructor TCableConstants.Destroy;
begin
    Reallocmem(FEpsR, 0);
    Reallocmem(FInsLayer, 0);
    Reallocmem(FDiaIns, 0);
    Reallocmem(FDiaCable, 0);
    inherited;
end;

end.
