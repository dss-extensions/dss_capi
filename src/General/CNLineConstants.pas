unit CNLineConstants;

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
    LineConstants,
    CableConstants;

type

    TCNLineConstants = class(TCableConstants)
    PRIVATE
        FkStrand: pIntegerArray;
        FDiaStrand: pDoubleArray;
        FGmrStrand: pDoubleArray;
        FRStrand: pDoubleArray;

    PUBLIC
        procedure Calc(f: Double; earthModel: Integer); OVERRIDE;

        constructor Create(NConductors: Integer);
        destructor Destroy; OVERRIDE;

        procedure SetkStrand(i: Integer; const Value: Integer);
        procedure SetDiaStrand(i, units: Integer; const Value: Double);
        procedure SetGmrStrand(i, units: Integer; const Value: Double);
        procedure SetRStrand(i, units: Integer; const Value: Double);
    end;

implementation

uses
    SysUtils,
    Math,
    Utilities;

procedure TCNLineConstants.SetkStrand(i: Integer; const Value: Integer);
begin
    if (i > 0) and (i <= numConductors) then
        FkStrand[i] := Value;
end;

procedure TCNLineConstants.SetDiaStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FDiaStrand[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.SetGmrStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FGmrStrand[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.SetRStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FRStrand[i] := Value * To_Per_Meter(units);
end;

procedure TCNLineConstants.Calc(f: Double; earthModel: Integer);
// Compute base Z and YC matrices in ohms/m for this frequency and earth impedance
var
    Zi, Zspacing: Complex;
    PowerFreq: Boolean;
    Lfactor: Complex;
    i, j: Integer;
    Dij, Yfactor: Double;
    ReducedSize: Integer;
    N, idxi, idxj: Integer;
    Zmat, Ztemp: TCMatrix;
    ResCN, RadCN: Double;
    GmrCN: Double;
    Denom, RadIn, RadOut: Double;
begin
    Frequency := f;  // this has side effects

    if assigned(FZreduced) then
    begin
        ReducedSize := FZreduced.order;
        FZreduced.Free;
    end
    else
        ReducedSize := 0;
    if assigned(FYCreduced) then
        FYCreduced.Free;
    FZreduced := NIL;
    FYCreduced := NIL;

    FZmatrix.Clear;
    FYCMatrix.Clear;

  // add concentric neutrals to the end of conductor list; they are always reduced
    N := numConductors + FNPhases;
    Zmat := TCMatrix.CreateMatrix(N);

    // For less than 1 kHz use GMR to better match published data
    LFactor := Cmplx(0.0, Fw * mu0 / twopi);
    if (f < 1000.0) and (f > 40.0) then
        PowerFreq := TRUE
    else
        PowerFreq := FALSE;

  // Self Impedances - CN cores and bare neutrals
    for i := 1 to numConductors do
    begin
        Zi := GetZint(i, earthModel);
        if PowerFreq then
        begin // for less than 1 kHz, use published GMR
            Zi.im := 0.0;
            Zspacing := Lfactor * ln(1.0 / FGMR[i]);  // use GMR
        end
        else
        begin
            Zspacing := Lfactor * ln(1.0 / Fradius[i]);
        end;
        Zmat[i, i] := Zi + Zspacing + GetZearth(i, i, earthModel);
    end;

  // CN self impedances
    for i := 1 to FNPhases do
    begin
        ResCN := FRstrand[i] / FkStrand[i];
        RadCN := 0.5 * (FDiaCable[i] - FDiaStrand[i]);
        GmrCN := Power(FGmrStrand[i] * FkStrand[i] * Power(RadCN, FkStrand[i] - 1.0),
            1.0 / FkStrand[i]);
        Zspacing := Lfactor * ln(1.0 / GmrCN);
        Zi := ResCN;
        idxi := i + numConductors;
        Zmat[idxi, idxi] := Zi + Zspacing + GetZearth(i, i, earthModel);
    end;

  // Mutual Impedances - between CN cores and bare neutrals
    for i := 1 to numConductors do
    begin
        for j := 1 to i - 1 do
        begin
            Dij := sqrt(sqr(Fx[i] - Fx[j]) + sqr(Fy[i] - Fy[j]));
            Zmat[i, j] := Lfactor * ln(1.0 / Dij) + GetZearth(i, j, earthModel);
            Zmat[j, i] := Zmat[i, j];
        end;
    end;

  // Mutual Impedances - CN to other CN, cores, and bare neutrals
    for i := 1 to FNPhases do
    begin
        idxi := i + numConductors;
        for j := 1 to i - 1 do
        begin  // CN to other CN
            idxj := j + numConductors;
            Dij := sqrt(sqr(Fx[i] - Fx[j]) + sqr(Fy[i] - Fy[j]));
            Zmat[idxi, idxj] := Lfactor * ln(1.0 / Dij) + GetZearth(i, j, earthModel);
            Zmat[idxj, idxi] := Zmat[idxi, idxj];
        end;
        for j := 1 to numConductors do
        begin // CN to cores and bare neutrals
            idxj := j;
            RadCN := 0.5 * (FDiaCable[i] - FDiaStrand[i]);
            if i = j then
            begin // CN to its own phase core
                Dij := RadCN;
            end
            else
            begin // CN to another phase or bare neutral
                Dij := sqrt(sqr(Fx[i] - Fx[j]) + sqr(Fy[i] - Fy[j]));
                Dij := Power(Power(Dij, FkStrand[i]) - Power(RadCN, FkStrand[i]), 1.0 / FkStrand[i]);
            end;
            Zmat[idxi, idxj] := Lfactor * ln(1.0 / Dij) + GetZearth(i, j, earthModel);
            Zmat[idxj, idxi] := Zmat[idxi, idxj];
        end;
    end;

  // reduce out the CN
    while Zmat.order > numConductors do
    begin
        Ztemp := Zmat.Kron(Zmat.order);
        Zmat.Free;
        Zmat := Ztemp;
    end;
    FZMatrix.CopyFrom(Zmat);
    Zmat.Free;

  // for shielded cables, build the capacitance matrix directly
  // assumes the insulation may lie between semicon layers
    for i := 1 to FNPhases do
    begin
        Yfactor := twopi * e0 * FEpsR[i] * Fw; // includes frequency so C==>Y
        RadOut := 0.5 * FDiaIns[i];
        RadIn := RadOut - FInsLayer[i];
        Denom := ln(RadOut / RadIn);
        FYCMatrix[i, i] := cmplx(0.0, Yfactor / Denom);
    end;

    if ReducedSize > 0 then
        Kron(ReducedSize);  // Was reduced so reduce again to same size

    // Else the Zmatrix is OK as last computed
    FRhoChanged := FALSE;
end;

constructor TCNLineConstants.Create(NConductors: Integer);
begin
    inherited Create(NConductors);
    FkStrand := Allocmem(Sizeof(FkStrand[1]) * numConductors);
    FDiaStrand := Allocmem(Sizeof(FDiaStrand[1]) * numConductors);
    FGmrStrand := Allocmem(Sizeof(FGmrStrand[1]) * numConductors);
    FRStrand := Allocmem(Sizeof(FRStrand[1]) * numConductors);
end;

destructor TCNLineConstants.Destroy;
begin
    Reallocmem(FkStrand, 0);
    Reallocmem(FDiaStrand, 0);
    Reallocmem(FGmrStrand, 0);
    Reallocmem(FRStrand, 0);
    inherited;
end;

end.
