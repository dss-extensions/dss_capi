unit TSLineConstants;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Arraydef,
    Ucmatrix,
    Ucomplex,
    LineUnits,
    LineConstants,
    CableConstants;

type

    TTSLineConstants = class(TCableConstants)
    PRIVATE
        FDiaShield: pDoubleArray;
        FTapeLayer: pDoubleArray;
        FTapeLap: pDoubleArray;

        function Get_DiaShield(i, units: Integer): Double;
        function Get_TapeLayer(i, units: Integer): Double;
        function Get_TapeLap(i: Integer): Double;

        procedure Set_DiaShield(i, units: Integer; const Value: Double);
        procedure Set_TapeLayer(i, units: Integer; const Value: Double);
        procedure Set_TapeLap(i: Integer; const Value: Double);
    PROTECTED

    PUBLIC
        procedure Calc(f: Double); OVERRIDE;

        constructor Create(NumConductors: Integer);
        destructor Destroy; OVERRIDE;

        property DiaShield[i, units: Integer]: Double READ Get_DiaShield WRITE Set_DiaShield;
        property TapeLayer[i, units: Integer]: Double READ Get_TapeLayer WRITE Set_TapeLayer;
        property TapeLap[i: Integer]: Double READ Get_TapeLap WRITE Set_TapeLap;
    end;

implementation

uses
    SysUtils,
    Math;

const
    RhoTS: Double = 2.3718e-8;  // for copper tape shield

function TTSLineConstants.Get_DiaShield(i, units: Integer): Double;
begin
    Result := FDiaShield^[i] * From_Meters(Units);
end;

function TTSLineConstants.Get_TapeLayer(i, units: Integer): Double;
begin
    Result := FTapeLayer^[i] * From_Meters(Units);
end;

function TTSLineConstants.Get_TapeLap(i: Integer): Double;
begin
    Result := FTapeLap^[i];
end;

procedure TTSLineConstants.Set_DiaShield(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FDiaShield^[i] := Value * To_Meters(units);
end;

procedure TTSLineConstants.Set_TapeLayer(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FTapeLayer^[i] := Value * To_Meters(units);
end;

procedure TTSLineConstants.Set_TapeLap(i: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FTapeLap^[i] := Value;
end;

procedure TTSLineConstants.Calc(f: Double);
{Compute base Z and YC matrices in ohms/m for this frequency and earth impedance}
var
    Zi, Zspacing: Complex;
    PowerFreq: Boolean;
    Lfactor: Complex;
    i, j: Integer;
    Dij, Yfactor: Double;
    ReducedSize: Integer;
    N, idxi, idxj: Integer;
    Zmat, Ztemp: TCMatrix;
    ResTS: Double;
    GmrTS: Double;
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
    N := FNumConds + FNumPhases;
    Zmat := TCMatrix.CreateMatrix(N);

  {For less than 1 kHz use GMR to better match published data}
    LFactor := Cmplx(0.0, Fw * mu0 / twopi);
    if (f < 1000.0) and (f > 40.0) then
        PowerFreq := TRUE
    else
        PowerFreq := FALSE;

  // Self Impedances - TS cores and bare neutrals
    for i := 1 to FNumConds do
    begin
        Zi := Get_Zint(i);
        if PowerFreq then
        begin // for less than 1 kHz, use published GMR
            Zi.im := 0.0;
            Zspacing := CmulReal(Lfactor, ln(1.0 / FGMR^[i]));  // use GMR
        end
        else
        begin
            Zspacing := CmulReal(Lfactor, ln(1.0 / Fradius^[i]));
        end;
        Zmat.SetElement(i, i, Cadd(Zi, Cadd(Zspacing, Get_Ze(i, i))));
    end;

  // TS self impedances
    for i := 1 to FNumPhases do
    begin
        ResTS := 0.3183 * RhoTS / (FDiaShield^[i] * FTapeLayer^[i] * sqrt(50.0 / (100.0 - FTapeLap^[i])));
        GmrTS := 0.5 * (FDiaShield^[i] - FTapeLayer^[i]);  // per Kersting, to center of TS
        Zspacing := CMulReal(Lfactor, ln(1.0 / GmrTS));
        Zi := cmplx(ResTS, 0.0);
        idxi := i + FNumConds;
        Zmat.SetElement(idxi, idxi, Cadd(Zi, Cadd(Zspacing, Get_Ze(i, i))));
    end;

  // Mutual Impedances - between TS cores and bare neutrals
    for i := 1 to FNumConds do
    begin
        for j := 1 to i - 1 do
        begin
            Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
            Zmat.SetElemSym(i, j, Cadd(Cmulreal(Lfactor, ln(1.0 / Dij)), Get_Ze(i, j)));
        end;
    end;

  // Mutual Impedances - TS to other TS, cores, and bare neutrals
    for i := 1 to FNumPhases do
    begin
        idxi := i + FNumConds;
        for j := 1 to i - 1 do
        begin  // TS to other TS
            idxj := j + FNumConds;
            Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
            Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0 / Dij)), Get_Ze(i, j)));
        end;
        for j := 1 to FNumConds do
        begin // CN to cores and bare neutrals
            idxj := j;
            GmrTS := 0.5 * (FDiaShield^[i] - FTapeLayer^[i]);  // per Kersting, to center of TS
            if i = j then
            begin // TS to its own phase core
                Dij := GmrTS;
            end
            else
            begin // TS to another phase or bare neutral
                Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
            end;
            Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0 / Dij)), Get_Ze(i, j)));
        end;
    end;

  // reduce out the tape shields
    while Zmat.Order > FNumConds do
    begin
        Ztemp := Zmat.Kron(Zmat.Order);
        Zmat.Free;
        Zmat := Ztemp;
    end;
    FZMatrix.CopyFrom(Zmat);
    Zmat.Free;

  // for shielded cables, build the capacitance matrix directly
  // assumes the insulation may lie between semicon layers
    for i := 1 to FNumPhases do
    begin
        Yfactor := twopi * e0 * FEpsR^[i] * Fw; // includes frequency so C==>Y
        RadOut := 0.5 * FDiaIns^[i];
        RadIn := RadOut - FInsLayer^[i];
        Denom := ln(RadOut / RadIn);
        FYCMatrix.SetElement(i, i, cmplx(0.0, Yfactor / Denom));
    end;

    if ReducedSize > 0 then
        Kron(ReducedSize);  // Was reduced so reduce again to same size

  {Else the Zmatrix is OK as last computed}
    FRhoChanged := FALSE;
end;

constructor TTSLineConstants.Create(NumConductors: Integer);
begin
    inherited Create(NumConductors);
    FDiaShield := Allocmem(Sizeof(FDiaShield^[1]) * FNumConds);
    FTapeLayer := Allocmem(Sizeof(FTapeLayer^[1]) * FNumConds);
    FTapeLap := Allocmem(Sizeof(FTapeLap^[1]) * FNumConds);
end;

destructor TTSLineConstants.Destroy;
begin
    Reallocmem(FDiaShield, 0);
    Reallocmem(FTapeLayer, 0);
    Reallocmem(FTapeLap, 0);
    inherited;
end;

initialization

end.
