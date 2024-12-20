unit CableConstants;

// ----------------------------------------------------------
// Copyright (c) 2018-2024, Paulo Meira
// Copyright (c) 2018-2024, DSS-Extensions contributors
// Copyright (c) 2008-2024, Electric Power Research Institute, Inc.
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
{$SCOPEDENUMS ON}
{$PUSH}
{$Z4} // keep enums as int32 values
    TConductorType = (
        INVALID = 0,
        CN = 1,
        TS = 2,
        Bare = 3
    );
    TConductorTypeArray = Array[1..100] of TConductorType;
    PConductorTypeArray = ^TConductorTypeArray;
{$SCOPEDENUMS OFF}
{$POP}

    TCableConstants = class(TLineConstants)
    PROTECTED
        FCondType: PConductorTypeArray; // Use as 1: CN, 2: TS, 3: Bare wire
        FEpsR: pDoubleArray;
        FInsLayer: pDoubleArray;
        FDiaIns: pDoubleArray;
        FDiaCable: pDoubleArray;

        // For CN
        FkStrand: pIntegerArray;
        FDiaStrand: pDoubleArray;
        FGmrStrand: pDoubleArray;
        FRStrand: pDoubleArray;
        semiconLayer: pBooleanArray;

        // For TS
        FDiaShield: pDoubleArray;
        FTapeLayer: pDoubleArray;
        FTapeLap: pDoubleArray;

    PUBLIC

        procedure Calc(f: Double; EarthModel: Integer); OVERRIDE;
        function ConductorsInSameSpace(var ErrorMessage: String): Boolean; OVERRIDE;
        procedure Kron(Norder: Integer); OVERRIDE; // don't reduce Y, it has zero neutral capacitance

        constructor Create(NConductors: Integer);
        destructor Destroy; OVERRIDE;

        procedure SetEpsR(i: Integer; const Value: Double);
        procedure SetInsLayer(i, units: Integer; const Value: Double);
        procedure SetDiaIns(i, units: Integer; const Value: Double);
        procedure SetDiaCable(i, units: Integer; const Value: Double);

        procedure SetkStrand(i: Integer; const Value: Integer);
        procedure SetDiaStrand(i, units: Integer; const Value: Double);
        procedure SetGmrStrand(i, units: Integer; const Value: Double);
        procedure SetRStrand(i, units: Integer; const Value: Double);
        procedure SetSemiconLayer(i: Integer; const Value: Boolean);

        procedure SetDiaShield(i, units: Integer; const Value: Double);
        procedure SetTapeLayer(i, units: Integer; const Value: Double);
        procedure SetTapeLap(i: Integer; const Value: Double);
        procedure SetCondType(i: Integer; const Value: TConductorType);
    end;

implementation

uses
    SysUtils;

const
    // For TS
    RhoTS: Double = 2.3718e-8;  // for copper tape shield

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
    if equivalentSpacing then
    begin
        for i := 1 to FNumConds do
        begin
            if i <= FNumPhases then
                Ri := FRadius[i]
            else
                Ri := 0.5 * FDiaCable[i];

            for j := i + 1 to FNumConds do
            begin
                if j <= FNumPhases then
                    Rj := FRadius[j]
                else
                    Rj := 0.5 * FDiaCable[j];

                if ((i <= FNumPhases) and (j > FNumPhases)) then
                    Dij := eqDistPhN
                else
                    Dij := eqDistPhPh;

                if (Dij < (Ri + Rj)) then
                begin
                    Result := true;
                    ErrorMessage := Format('Cable conductors %d and %d occupy the same space.', [i, j]);
                    Exit;
                end;
            end;
        end;
        Exit;
    end;


    for i := 1 to numConductors do
    begin
        if i <= nPhases then
            Ri := FRadius[i]
        else
            Ri := 0.5 * FDiaCable[i];
        for j := i + 1 to numConductors do
        begin
            if j <= nPhases then
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

    FCondType := Allocmem(Sizeof(TConductorType) * FNumConds);

    FEpsR := Allocmem(Sizeof(Double) * numConductors);
    FInsLayer := Allocmem(Sizeof(Double) * numConductors);
    FDiaIns := Allocmem(Sizeof(Double) * numConductors);
    FDiaCable := Allocmem(Sizeof(Double) * numConductors);

    FkStrand := Allocmem(Sizeof(Integer) * numConductors);
    FDiaStrand := Allocmem(Sizeof(Double) * numConductors);
    FGmrStrand := Allocmem(Sizeof(Double) * numConductors);
    FRStrand := Allocmem(Sizeof(Double) * numConductors);
    semiconLayer := Allocmem(Sizeof(Boolean) * numConductors);

    FDiaShield := Allocmem(Sizeof(Double) * numConductors);
    FTapeLayer := Allocmem(Sizeof(Double) * numConductors);
    FTapeLap := Allocmem(Sizeof(Double) * numConductors);
end;

destructor TCableConstants.Destroy;
begin
    Reallocmem(FCondType, 0);

    Reallocmem(FEpsR, 0);
    Reallocmem(FInsLayer, 0);
    Reallocmem(FDiaIns, 0);
    Reallocmem(FDiaCable, 0);

    Reallocmem(FkStrand, 0);
    Reallocmem(FDiaStrand, 0);
    Reallocmem(FGmrStrand, 0);
    Reallocmem(FRStrand, 0);
    Reallocmem(semiconLayer, 0);

    Reallocmem(FDiaShield, 0);
    Reallocmem(FTapeLayer, 0);
    Reallocmem(FTapeLap, 0);

    inherited;
end;

procedure TCableConstants.SetkStrand(i: Integer; const Value: Integer);
begin
    if (i > 0) and (i <= numConductors) then
        FkStrand[i] := Value;
end;

procedure TCableConstants.SetSemiconLayer(i: Integer; const Value: Boolean);
begin
    if (i > 0) and (i <= FNumConds) then
        semiconLayer[i] := Value;
end;

procedure TCableConstants.SetDiaStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FDiaStrand[i] := Value * To_Meters(units);
end;

procedure TCableConstants.SetGmrStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FGmrStrand[i] := Value * To_Meters(units);
end;

procedure TCableConstants.SetRStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FRStrand[i] := Value * To_Per_Meter(units);
end;

procedure TCableConstants.Calc(f: Double; earthModel: Integer);
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
    Denom, RadIn, RadOut: Double;

    // For CN
    ResCN, RadCN, RadStrand: Double;
    GmrCN: Double;

    // For TS
    ResTS: Double;
    GmrTS: Double;

    function GetDij(i, j: Integer): Double;
    begin
        if not FEquivalentSpacing then
        begin
            Result := sqrt(sqr(Fx[i] - Fx[j]) + sqr(Fy[i] - Fy[j]));
            Exit;
        end;

        if ((j <= FNumPhases) and (i > FNumPhases)) then
        begin
            Result := eqDistPhN;
            Exit;
        end;

        Result := eqDistPhPh; // including N-N conductors
    end;

begin
    SetFrequency(f);  // this has side effects

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

    // add concentric neutrals or tape shields to the end of conductor list; they are always reduced
    N := numConductors + nPhases;
    Zmat := TCMatrix.CreateMatrix(N);

    // For less than 1 kHz use GMR to better match published data
    LFactor := Cmplx(0.0, Fw * mu0 / twopi);
    if (f < 1000.0) and (f > 40.0) then
        PowerFreq := TRUE
    else
        PowerFreq := FALSE;

    // Self Impedances - CN/TS cores and bare neutrals
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

    // CN/TS self impedances
    for i := 1 to nPhases do
    begin
        case FCondType[i] of
            TConductorType.CN:
            begin
                ResCN := FRstrand[i] / FkStrand[i];
                RadCN := 0.5 * (FDiaCable[i] - FDiaStrand[i]);
                GmrCN := Power(FGmrStrand[i] * FkStrand[i] * Power(RadCN, FkStrand[i] - 1.0), 1.0 / FkStrand[i]);
                Zspacing := Lfactor * ln(1.0 / GmrCN);
                Zi := ResCN;
                idxi := i + numConductors;
                Zmat[idxi, idxi] := Zi + Zspacing + GetZearth(i, i, earthModel);
            end;
            TConductorType.TS:
            begin
                ResTS := 0.3183 * RhoTS / (FDiaShield[i] * FTapeLayer[i] * sqrt(50.0 / (100.0 - FTapeLap[i])));
                GmrTS := 0.5 * (FDiaShield[i] - FTapeLayer[i]);  // per Kersting, to center of TS
                Zspacing := Lfactor * ln(1.0 / GmrTS);
                Zi := ResTS;
                idxi := i + numConductors;
                Zmat[idxi, idxi] := Zi + Zspacing + GetZearth(i, i, earthModel);
            end;
        end;
    end;

    // Mutual Impedances - between CN cores and bare neutrals
    for i := 1 to numConductors do
    begin
        for j := 1 to i - 1 do
        begin
            Dij := GetDij(i, j);
            Zmat[i, j] := Lfactor * ln(1.0 / Dij) + GetZearth(i, j, earthModel);
            Zmat[j, i] := Zmat[i, j];
        end;
    end;

    // Mutual Impedances - CN/TS to other CN/TS, cores, and bare neutrals
    for i := 1 to nPhases do
    begin
        idxi := i + numConductors;
        for j := 1 to i - 1 do
        begin  // CN to other CN
            idxj := j + numConductors;
            Dij := GetDij(i, j);
            Zmat[idxi, idxj] := Lfactor * ln(1.0 / Dij) + GetZearth(i, j, earthModel);
            Zmat[idxj, idxi] := Zmat[idxi, idxj];
        end;

        for j := 1 to numConductors do
        begin // CN/TS to cores and bare neutrals
            idxj := j;
            case FCondType[i] of
                TConductorType.CN:
                begin
                    RadCN := 0.5 * (FDiaCable[i] - FDiaStrand[i]);
                    if i = j then
                    begin // CN to its own phase core
                        Dij := RadCN;
                    end
                    else
                    begin // CN to another phase or bare neutral
                        Dij := GetDij(i, j);
                        Dij := Power(Power(Dij, FkStrand[i]) - Power(RadCN, FkStrand[i]), 1.0 / FkStrand[i]);
                    end;
                end;
                TConductorType.TS:
                begin
                    GmrTS := 0.5 * (FDiaShield[i] - FTapeLayer[i]);  // per Kersting, to center of TS
                    if i = j then
                    begin // TS to its own phase core
                        Dij := GmrTS;
                    end
                    else
                    begin // TS to another phase or bare neutral
                        Dij := GetDij(i, j);
                    end;
                end;
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
    // assumes the insulation may lie between semiconLayer layers
    for i := 1 to nPhases do
    begin
        Yfactor := twopi * e0 * FEpsR[i] * Fw; // includes frequency so C==>Y
        RadOut := 0.5 * FDiaIns[i];
        RadIn := RadOut - FInsLayer[i];
        case FCondType[i] of
            TConductorType.CN:
            begin
                if semiconLayer[i] then
                begin
                    // semiconLayer layer (default)
                    Denom := ln(RadOut / RadIn);
                end
                else
                begin
                    // No semiconLayer layer (Synergi and Kersting/Kerestes' book)
                    RadCN := 0.5 * (FDiaCable[i] - FDiaStrand[i]);
                    RadStrand := 0.5 * FDiaStrand[i];
                    Denom := ln(RadCN / RadIn) - (1 / FkStrand[i]) * ln(FkStrand[i] * RadStrand / RadCN);
                end;
            end;
            TConductorType.TS:
            begin
                Denom := ln(RadOut / RadIn);
            end;
        end;
        FYCMatrix[i, i] := cmplx(0.0, Yfactor / Denom);
    end;

    if ReducedSize > 0 then
        Kron(ReducedSize);  // Was reduced so reduce again to same size

    // Else the Zmatrix is OK as last computed
    rhoChanged := FALSE;
end;

procedure TCableConstants.SetDiaShield(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FDiaShield[i] := Value * To_Meters(units);
end;

procedure TCableConstants.SetTapeLayer(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FTapeLayer[i] := Value * To_Meters(units);
end;

procedure TCableConstants.SetTapeLap(i: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FTapeLap[i] := Value;
end;

procedure TCableConstants.SetCondType(i: Integer; const Value: TConductorType);
begin
    if (i > 0) and (i <= numConductors) then
        FCondType[i] := Value;
end;

end.
