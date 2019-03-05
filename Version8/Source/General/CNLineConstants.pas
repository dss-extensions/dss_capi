unit CNLineConstants;

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

    TCNLineConstants = class(TCableConstants)
    PRIVATE
        FkStrand: pIntegerArray;
        FDiaStrand: pDoubleArray;
        FGmrStrand: pDoubleArray;
        FRStrand: pDoubleArray;

        function Get_kStrand(i: Integer): Integer;
        function Get_DiaStrand(i, units: Integer): Double;
        function Get_GmrStrand(i, units: Integer): Double;
        function Get_RStrand(i, units: Integer): Double;

        procedure Set_kStrand(i: Integer; const Value: Integer);
        procedure Set_DiaStrand(i, units: Integer; const Value: Double);
        procedure Set_GmrStrand(i, units: Integer; const Value: Double);
        procedure Set_RStrand(i, units: Integer; const Value: Double);
    PROTECTED

    PUBLIC
        procedure Calc(f: Double); OVERRIDE;

        constructor Create(NumConductors: Integer);
        destructor Destroy; OVERRIDE;

        property kStrand[i: Integer]: Integer READ Get_kStrand WRITE Set_kStrand;
        property DiaStrand[i, units: Integer]: Double READ Get_DiaStrand WRITE Set_DiaStrand;
        property GmrStrand[i, units: Integer]: Double READ Get_GmrStrand WRITE Set_GmrStrand;
        property RStrand[i, units: Integer]: Double READ Get_RStrand WRITE Set_RStrand;
    end;

implementation

uses
    SysUtils,
    Math,
    Utilities;

function TCNLineConstants.Get_kStrand(i: Integer): Integer;
begin
    Result := FkStrand^[i];
end;

function TCNLineConstants.Get_DiaStrand(i, units: Integer): Double;
begin
    Result := FDiaStrand^[i] * From_Meters(Units);
end;

function TCNLineConstants.Get_GmrStrand(i, units: Integer): Double;
begin
    Result := FGmrStrand^[i] * From_Meters(Units);
end;

function TCNLineConstants.Get_RStrand(i, units: Integer): Double;
begin
    Result := FRStrand^[i] * From_Per_Meter(Units);
end;

procedure TCNLineConstants.Set_kStrand(i: Integer; const Value: Integer);
begin
    if (i > 0) and (i <= FNumConds) then
        FkStrand^[i] := Value;
end;

procedure TCNLineConstants.Set_DiaStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FDiaStrand^[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.Set_GmrStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FGmrStrand^[i] := Value * To_Meters(units);
end;

procedure TCNLineConstants.Set_RStrand(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FRStrand^[i] := Value * To_Per_Meter(units);
end;

procedure TCNLineConstants.Calc(f: Double);
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
    ResCN, RadCN: Double;
    GmrCN: Double;
    Denom, RadIn, RadOut: Double;
  {$IFDEF ANDREA}
  {****} DumpFile: TextFile;
  {$ENDIF}
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

  // Self Impedances - CN cores and bare neutrals
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

  // CN self impedances
    for i := 1 to FNumPhases do
    begin
        ResCN := FRstrand^[i] / FkStrand^[i];
        RadCN := 0.5 * (FDiaCable^[i] - FDiaStrand^[i]);
        GmrCN := Power(FGmrStrand^[i] * FkStrand^[i] * Power(RadCN, FkStrand^[i] - 1.0),
            1.0 / FkStrand^[i]);
        Zspacing := CMulReal(Lfactor, ln(1.0 / GmrCN));
        Zi := cmplx(ResCN, 0.0);
        idxi := i + FNumConds;
        Zmat.SetElement(idxi, idxi, Cadd(Zi, Cadd(Zspacing, Get_Ze(i, i))));
    end;

  // Mutual Impedances - between CN cores and bare neutrals
    for i := 1 to FNumConds do
    begin
        for j := 1 to i - 1 do
        begin
            Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
            Zmat.SetElemSym(i, j, Cadd(Cmulreal(Lfactor, ln(1.0 / Dij)), Get_Ze(i, j)));
        end;
    end;

  // Mutual Impedances - CN to other CN, cores, and bare neutrals
    for i := 1 to FNumPhases do
    begin
        idxi := i + FNumConds;
        for j := 1 to i - 1 do
        begin  // CN to other CN
            idxj := j + FNumConds;
            Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
            Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0 / Dij)), Get_Ze(i, j)));
        end;
        for j := 1 to FNumConds do
        begin // CN to cores and bare neutrals
            idxj := j;
            RadCN := 0.5 * (FDiaCable^[i] - FDiaStrand^[i]);
            if i = j then
            begin // CN to its own phase core
                Dij := RadCN;
            end
            else
            begin // CN to another phase or bare neutral
                Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
                Dij := Power(Power(Dij, FkStrand^[i]) - Power(RadCN, FkStrand^[i]),
                    1.0 / FkStrand^[i]);
            end;
            Zmat.SetElemSym(idxi, idxj, Cadd(Cmulreal(Lfactor, ln(1.0 / Dij)), Get_Ze(i, j)));
        end;
    end;

  {$IFDEF ANDREA}
//***** Special for Andrea to see 6x6 matrix before it is reduced
    Assignfile(DumpFile, 'CNData-1.txt');
    Rewrite(Dumpfile);
    Writeln(DumpFile, 'Before Reduction');
    DumpComplexMatrix(DumpFile, Zmat);
//*****
  {$ENDIF}

  // reduce out the CN
    while Zmat.Order > FNumConds do
    begin
        Ztemp := Zmat.Kron(Zmat.Order);
        Zmat.Free;
        Zmat := Ztemp;
    end;
    FZMatrix.CopyFrom(Zmat);
    Zmat.Free;

  {$IFDEF ANDREA}
//*****    Special for Andrea
    Writeln(DumpFile, 'After Reduction');
    DumpComplexMatrix(DumpFile, FZMatrix);
    CloseFile(DumpFile);
    FireOffEditor('CNData-1.txt');
//*****
  {$ENDIF}

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

constructor TCNLineConstants.Create(NumConductors: Integer);
begin
    inherited Create(NumConductors);
    FkStrand := Allocmem(Sizeof(FkStrand^[1]) * FNumConds);
    FDiaStrand := Allocmem(Sizeof(FDiaStrand^[1]) * FNumConds);
    FGmrStrand := Allocmem(Sizeof(FGmrStrand^[1]) * FNumConds);
    FRStrand := Allocmem(Sizeof(FRStrand^[1]) * FNumConds);
end;

destructor TCNLineConstants.Destroy;
begin
    Reallocmem(FkStrand, 0);
    Reallocmem(FDiaStrand, 0);
    Reallocmem(FGmrStrand, 0);
    Reallocmem(FRStrand, 0);
    inherited;
end;

initialization

end.
