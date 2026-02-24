unit LineConstants;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

// Manages the geometry data and calculates the impedance matrices for an overhead line

// Usage: Create with Number of conductors you want
//        Specify the number of phases. The first conductors you define with
//        be the phases. Other conductors may be considered neutral.
//
//        Uses GMR for power frequency calcs so that answers match published
//        data.
//
//        You only have to set R or GMR. The other will default. However, you should set
//        both for better accuracy.
//
//        When you ask for Zmatrix or YCmatrix you get the full matrix unless you have executed
//        a Kron reduction or Reduce function. Reduce eleminates all non phases. If you
//        want the full detailed model, DO NOT REDUCE!

interface

uses
    CAPI_Types,
    Ucmatrix,
    UComplex, DSSUcomplex,
    LineUnits;

type

    // This class returns a matrix ordered by phases first then remaining conductors
    // Assumes phases are defined first

    TLineConstants = class(TObject)
    PROTECTED
        FData: pDouble;

        // Memory for the arrays below is shared in FData above;

        FX: pDoubleArray;
        FY: pDoubleArray;

        equivalentSpacing: Boolean;

        FRdc: pDoubleArray;   // ohms/m
        FRac: pDoubleArray;   // ohms/m
        FGMR: pDoubleArray;   // m
        Fradius: pDoubleArray;
        Fcapradius: pDoubleArray;  // if different than radius; defaults to radius
                                   // Primarily for bundled conductors

        FZmatrix: TCmatrix;   // in ohms/m
        FYCmatrix: TCmatrix;   // siemens/m   --- jwC

        FZreduced: TCMatrix;  // These two do not exist until Kron Reduction
        FYCreduced: TCMatrix;  // is executed

        FFrequency: Double;  // Frequency for which impedances are computed
        Fw: Double;  // 2piF
        Fme: Complex; // factor for earth impedance
        rhoChanged: Boolean;
        epsRMedium: Double;  // unit-less
        userHeightUnit: Integer;

        function GetZearth(i, j, EarthModel: Integer): Complex;
        function GetZint(i, EarthModel: Integer): Complex;
        procedure SetFrequency(const Value: Double);
    PUBLIC
        heightOffset: Double;  // stored in meters
        eqDistPhPh, eqDistPhN, avgPhaseHeight, avgNeutralHeight: Double; // stored in meters

        FrhoEarth: Double;  // ohm-m
        nPhases: Integer;
        numConductors: Integer;

        procedure SetRhoEarth(const Value: Double);  // m
        function ConductorsInSameSpace(var ErrorMessage: String): Boolean; VIRTUAL;
        procedure Calc(f: Double; EarthModel: Integer); VIRTUAL; // force a calc of impedances
        procedure Kron(Norder: Integer); VIRTUAL; // Performs a Kron reduction leaving first Norder  rows
        procedure Reduce();  // Kron reduce to Numphases only

        procedure SetX(i, units: Integer; const Value: Double);
        procedure SetY(i, units: Integer; const Value: Double);
        procedure SetRdc(i, units: Integer; const Value: Double);
        procedure SetRac(i, units: Integer; const Value: Double);
        procedure SetRadius(i, units: Integer; const Value: Double);
        // This allows you to compute capacitance using a different radius -- for bundled conductors
        procedure SetCapradius(i, units: Integer; const Value: Double);
        procedure SetGMR(i, units: Integer; const Value: Double);
        
        // These two functions will auto recalc the impedance matrices if frequency is different
        // Converts to desired units when executed; Returns Pointer to Working Verstion
        function GetZMatrix(f, Lngth: Double; Units, EarthModel: Integer): Tcmatrix;
        function GetYCMatrix(f, Lngth: Double; Units: Integer): Tcmatrix;

        procedure SetHeightOffset(const Value: Double);
        procedure SetUserHeightUnit(const Value: Integer);
        procedure SetEquivalentSpacing(const Value: Boolean);
        procedure SetEpsRMedium(const Value: Double);
        function GetHeightOffset(): Double;        
        function GetEpsRMedium(): Double;
        function GetUserHeightUnit(): Integer;

        constructor Create(NConductors: Integer);
        destructor Destroy; OVERRIDE;

    end;

const //TODO: precision
    e0: Double = 8.854e-12;  // dielectric constant  F/m
    mu0: Double = 12.56637e-7; // hy/m
    Twopi: Double = 6.283185307;

implementation

uses
    DSSGlobals,
    mathutil,
    sysutils,
    math,
    DSSClass,
    DSSHelper;

const
    C1_j1: Complex = (re: 1.0; im: 1.0);
    b1: Double = 1.0 / (3.0 * sqrt(2.0));
    b2: Double = 1.0 / 16.0;
    b3: Double = (1.0 / (3.0 * sqrt(2.0))) / 3.0 / 5.0; // b1...
    b4: Double = (1.0 / 16.0) / 4.0 / 6.0; // b2...
    d2: Double = (1.0 / 16.0) * pi / 4.0; // b2...
    d4: Double = ((1.0 / 16.0) / 4.0 / 6.0) * pi / 4.0; // b4...
    c2: Double = 1.3659315;
    c4: Double = (1.3659315) + 1.0 / 4.0 + 1.0 / 6.0; // c2...


procedure TLineConstants.Calc(f: Double; EarthModel: Integer);
// Compute base Z and YC matrices in ohms/m for this frequency and earth impedance
var
    Zi, Zspacing: Complex;
    PowerFreq: Boolean;
    Lfactor: Complex;
    i, j: Integer;
    Dij, Dijp, Pfactor: Double;
    ReducedSize: Integer;

begin
    // rhoEarth := rho;
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

    // For less than 1 kHz use GMR to better match published data

    LFactor := Cmplx(0.0, Fw * mu0 / twopi);
    if (f < 1000.0) and (f > 40.0) then
        PowerFreq := TRUE
    else
        PowerFreq := FALSE;

    // Self Impedances

    for i := 1 to numConductors do
    begin
        Zi := GetZint(i, EarthModel);
        if PowerFreq then
        begin // for less than 1 kHz, use published GMR
            Zi.im := 0.0;
            Zspacing := Lfactor * ln(1.0 / FGMR[i]);  // use GMR
        end
        else
        begin
            Zspacing := Lfactor * ln(1.0 / Fradius[i]);
        end;

        FZmatrix[i, i] := Zi + Zspacing + GetZearth(i, i, EarthModel);

    end;

    // Mutual Impedances

    for i := 1 to numConductors do
    begin
        for j := 1 to i - 1 do
        begin
            if not equivalentSpacing then
            begin
                Dij := sqrt(sqr(Fx[i] - Fx[j]) + sqr(Fy[i] - Fy[j]));
            end
            else
            begin
                if ((j <= nPhases) and (i > nPhases)) then
                    Dij := eqDistPhN // EqDistPhN
                else
                    Dij := eqDistPhPh;  // EqDistPhPh (including N-N conductorss)
            end;
            FZmatrix[i, j] := Lfactor * ln(1.0 / Dij) + GetZearth(i, j, EarthModel);
            FZmatrix[j, i] := FZmatrix[i, j];
        end;
    end;

    // Capacitance Matrix

    Pfactor := -1.0 / twopi / (e0 * epsRMedium) / Fw; // include frequency   // epsRMedium = 0.9993366876323544 to match Synergi

    // Construct P matrix and then invert

    // Self uses capradius, which defaults to actual conductor radius. But
    // in case of bundled conductors can be specified different in Wiredata.

    for i := 1 to numConductors do
    begin
        if not equivalentSpacing then
        begin
            if Fcapradius[i] < 0 then
                FYCMatrix[i, i] := cmplx(0.0, pfactor * ln(2.0 * Fy[i] / Fradius[i]))
            else
                FYCMatrix[i, i] := cmplx(0.0, pfactor * ln(2.0 * Fy[i] / Fcapradius[i]));

            continue;
        end;
        
        if (i > nPhases) then
            FYCMatrix.SetElement(i, i, cmplx(0.0, pfactor * ln(2.0 * avgNeutralHeight / Fcapradius[i])))
        else
            FYCMatrix.SetElement(i, i, cmplx(0.0, pfactor * ln(2.0 * avgPhaseHeight / Fcapradius[i])));
    end;

    for i := 1 to numConductors do
    begin
        for j := 1 to i - 1 do
        begin
            if not equivalentSpacing then
            begin
                Dij := sqrt(sqr(Fx[i] - Fx[j]) + sqr(Fy[i] - Fy[j]));
                Dijp := sqrt(sqr(Fx[i] - Fx[j]) + sqr(Fy[i] + Fy[j])); // distance to image j
            end
            else
            begin
                if ((j <= nPhases) and (i > nPhases)) then
                    Dij := eqDistPhN // EqDistPhN
                else
                    Dij := eqDistPhPh;  // EqDistPhPh (including N-N conductorss)

                if ((j <= nPhases) and (i > nPhases)) then
                    Dijp := (avgPhaseHeight + avgNeutralHeight) // AvgHeightPhase + AvgHeightNeutral
                else
                if ((i <= nPhases) and (j <= nPhases)) then
                    Dijp := (2 * avgPhaseHeight) // 2 * AvgHeightPhase
                else
                    Dijp := (2 * avgNeutralHeight) // 2 * AvgHeightNeutral
            end;

            FYCMatrix[i, j] := cmplx(0.0, pfactor * ln(Dijp / Dij));
            FYCMatrix[j, i] := FYCMatrix[i, j];
        end;
    end;

    FYCMatrix.Invert; // now should be nodal C matrix

    if ReducedSize > 0 then
        Kron(ReducedSize);  // Was reduced so reduce again to same size

    // Else the Zmatrix is OK as last computed

    rhoChanged := FALSE;
end;

function TLineConstants.ConductorsInSameSpace(var ErrorMessage: String): Boolean;
var
    i, j: Integer;
    Dij: Double;
begin
    // Check all conductors to make sure none occupy the same space or are defined at 0,0
    Result := FALSE;

    if equivalentSpacing then
    begin
        // Check for 0 Y coordinate
        if (avgPhaseHeight <= 0.0) or (avgNeutralHeight <= 0.0) then
        begin
            Result := true;
            ErrorMessage := 'Conductor average heights (overhead equivalent spacing) must be > 0.';
            Exit
        end;
        // Check for overlapping conductors
        for i := 1 to numConductors do
        begin
            for j := i + 1 to numConductors do
            begin
                if ((i <= nPhases) and (j > nPhases)) then
                    Dij := eqDistPhN
                else
                    Dij := eqDistPhPh;

                if (Dij < (Fradius[i] + Fradius[j])) then
                begin
                    Result := true;
                    ErrorMessage := Format('Conductors %d and %d occupy the same space.', [i, j]);
                    Exit;
                end;
            end;
        end;
        Exit;
    end;

    // Check for 0 Y coordinate
    for i := 1 to numConductors do
    begin
        if (FY[i] <= 0.0) then
        begin
            Result := TRUE;
            ErrorMessage := Format('Conductor %d height must be  > 0. ', [i]);
            Exit
        end;
    end;

    // Check for overlapping conductors
    for i := 1 to numConductors do
    begin
        for j := i + 1 to numConductors do
        begin
            Dij := Sqrt(SQR(FX[i] - FX[j]) + SQR(FY[i] - FY[j]));
            if (Dij < (Fradius[i] + Fradius[j])) then
            begin
                Result := TRUE;
                ErrorMessage := Format('Conductors %d and %d occupy the same space.', [i, j]);
                Exit;
            end;
        end;
    end;
end;

constructor TLineConstants.Create(NConductors: Integer);
var
    i: Integer;
begin
    numConductors := NConductors;
    nPhases := numConductors;

    // Data for FX, FY, FGMR, Fradius, Fcapradius, FRdc, FRac, 
    // FZMatrix, FYCMatrix
    FData := Allocmem(Sizeof(Double) * (numConductors * 7 + numConductors * numConductors * (2 * 2)));

    FX := pDoubleArray(FData);
    FY := pDoubleArray(FData + numConductors);
    FGMR := pDoubleArray(FData + numConductors * 2);
    Fradius := pDoubleArray(FData + numConductors * 3);
    Fcapradius := pDoubleArray(FData + numConductors * 4);
    FRdc := pDoubleArray(FData + numConductors * 5);
    FRac := pDoubleArray(FData + numConductors * 6);

    equivalentSpacing := false;

    // Initialize to  not set
    for i := 1 to numConductors do
        FGMR[i] := -1.0;
    for i := 1 to numConductors do
        Fradius[i] := -1.0;
    for i := 1 to numConductors do
        Fcapradius[i] := -1.0;
    for i := 1 to numConductors do
        FRdc[i] := -1.0;

    FZMatrix := TCMatrix.CreateMatrixInplace(numConductors, pComplex(FData + numConductors * 7));
    FYCMatrix := TCMatrix.CreateMatrixInPlace(numConductors, pComplex(FData + numConductors * 7 + numConductors * numConductors * 2));

    FFrequency := -1.0;  // not computed
    FrhoEarth := 100.0;  // default value

    epsRMedium := 1.0;  // default value should be 1.0
    heightOffset := 0.0;  // default value should be 0.0
    userHeightUnit := UNITS_M;

    rhoChanged := true; // using for both rho and epsilon_r

    FZreduced := NIL;
    FYCreduced := NIL;
end;

destructor TLineConstants.Destroy;
begin
    if assigned(FZmatrix) then
        FZmatrix.Free;
    if assigned(FYCmatrix) then
        FYCmatrix.Free;
    if assigned(FZreduced) then
        FZreduced.Free;
    if assigned(FYCreduced) then
        FYCreduced.Free;

    Reallocmem(FData, 0);

    inherited;
end;

function TLineConstants.GetYCMatrix(f, Lngth: Double; Units: Integer): Tcmatrix;
// Makes a new YCmatrix and correct for lengths and units as it copies
// Uses the reduced Zmatrix by default if it exists
var
    Newsize, i: Integer;
    UnitLengthConversion: Double;
    YC: TCMatrix;
    YCValues: pComplexArray;
begin
    if assigned(FYCreduced) then
        YC := FYCReduced
    else
        YC := FYCmatrix;

    NewSize := YC.order;
    Result := TCmatrix.CreateMatrix(Newsize);

    Result.CopyFrom(YC);
    YCvalues := Result.GetValuesArrayPtr(Newsize);
    UnitLengthConversion := From_per_meter(Units) * lngth;
    for i := 1 to NewSize * NewSize do
        YCValues[i] *= UnitLengthConversion;
end;

function TLineConstants.GetZearth(i, j, EarthModel: Integer): Complex;
var
    LnArg, hterm, xterm: Complex;
    mij, thetaij, Dij, Fyi, Fyj, Fxi_Fxj: Double;
    term1, term2, term3, term4, term5: Double;
begin
    Fyi := Abs(Fy[i]);
    Fyj := Abs(Fy[j]);

    if not equivalentSpacing then
        Fyi := Abs(Fy[i])
    else
    if i <= nPhases then
        Fyi := Abs(avgPhaseHeight)
    else
        Fyi := Abs(avgNeutralHeight);

    if not equivalentSpacing then
        Fyj := Abs(Fy[j])
    else
    if j <= nPhases then
        Fyj := Abs(avgPhaseHeight)
    else
        Fyj := Abs(avgNeutralHeight);

    // If the spacing uses equivalent distance, assume the equivalent distance is on the X axis.
    if not equivalentSpacing then
        Fxi_Fxj := Fx[i] - Fx[j]
    else
    if ((i <= nPhases) and (j <= nPhases)) or ((i > nPhases) and (j > nPhases)) then
        Fxi_Fxj := eqDistPhPh
    else
        Fxi_Fxj := eqDistPhN;

    case EarthModel of

        SIMPLECARSON:
        begin
            Result := cmplx(Fw * Mu0 / 8.0, (Fw * Mu0 / twopi) * ln(658.8530451057239 * sqrt(FrhoEarth / FFrequency)));
 // {****}             WriteDLLDebugFile(Format('Simple: Z(%d,%d) = %.8g +j %.8g',[i,j, Result.re, result.im]));
        end;

        FULLCARSON:
        begin
            // notation from Tleis book Power System Modelling and Fault Analysis
            if i = j then
            begin
                thetaij := 0.0;
                Dij := 2.0 * Fyi;
            end
            else
            begin
                Dij := sqrt(sqr(Fyi + Fyj) + sqr(Fxi_Fxj));
                thetaij := ArcCos((Fyi + Fyj) / Dij);
            end;
            mij := 2.8099e-3 * Dij * sqrt(FFrequency / FrhoEarth);

            Result.re := pi / 8.0 - b1 * mij * cos(thetaij) + b2 * sqr(mij) * (ln(exp(c2) / mij) * cos(2.0 * thetaij) + thetaij * sin(2.0 * thetaij)) + b3 * mij * mij * mij * cos(3.0 * thetaij) - d4 * mij * mij * mij * mij * cos(4.0 * thetaij);

            term1 := 0.5 * ln(1.85138 / mij);
            term2 := b1 * mij * cos(thetaij);
            term3 := -d2 * sqr(mij) * cos(2.0 * thetaij);
            term4 := b3 * mij * mij * mij * cos(3.0 * thetaij);
            term5 := -b4 * mij * mij * mij * mij * (ln(exp(c4) / mij) * cos(4.0 * thetaij) + thetaij * sin(4.0 * thetaij));
            Result.im := term1 + term2 + term3 + term4 + term5;
            Result.im := Result.im + 0.5 * ln(Dij);  // correction term to work with DSS structure

            Result := Result * (Fw * Mu0 / pi);

 //  {****}         WriteDLLDebugFile(Format('Full: Z(%d,%d) = %.8g +j %.8g; Dij=%.8g, thetaij=%.8g, mij=%.8g, Terms= %.8g, %.8g, %.8g, %.8g, %.8g',[i,j, Result.re, result.im, Dij, thetaij*180.0/pi, mij, term1, term2, term3, term4, term5]));

        end;

        DERI:
        begin
            if i <> j then
            begin
                hterm := (Fyi + Fyj) + Cinv(Fme) * 2.0;
                xterm := Fxi_Fxj;
                LnArg := Csqrt(hterm * hterm + xterm * xterm);
                Result := Cmplx(0.0, Fw * Mu0 / twopi) * Cln(lnArg);
            end
            else
            begin
                hterm := Fyi + Cinv(Fme);
                Result := Cmplx(0.0, Fw * Mu0 / twopi) * Cln(hterm * 2.0);
            end;
 // {****}          WriteDLLDebugFile(Format('Deri: Z(%d,%d) = %.8g +j %.8g; hterm= %.8g + j %.8g',[i,j, Result.re, result.im, hterm.re, hterm.im]));
        end;
    end;
end;

function TLineConstants.GetZint(i, EarthModel: Integer): Complex;
var
    Alpha, I0I1: Complex;
begin
    case EarthModel of
        SIMPLECARSON:
        begin
            Result := cmplx(FRac[i], Fw * Mu0 / (8 * pi));
        end;
        FULLCARSON:
        begin // no skin effect
            Result := cmplx(FRac[i], Fw * Mu0 / (8 * pi));
        end;
        DERI:
        begin // with skin effect model
            // Assume round conductor
            Alpha := c1_j1 * sqrt(FFrequency * mu0 / FRDC[i]);
            if Cabs(Alpha) > 35.0 then
                I0I1 := 1
            else
                I0I1 := Bessel_I0(Alpha) / Bessel_I1(Alpha);

            Result := C1_j1 * I0I1 * (Sqrt(FRdc[i] * FFrequency * mu0) / 2.0);
        end;
    end;
end;

function TLineConstants.GetZMatrix(f, Lngth: Double; Units, EarthModel: Integer): Tcmatrix;
// Makes a new Zmatrix and correct for lengths and units as it copies
// Uses the reduced Zmatrix by default if it exists
var
    Newsize, i: Integer;
    UnitLengthConversion: Double;
    Z: TCMatrix;
    ZValues: pComplexArray;
begin
    if (F <> FFrequency) or rhoChanged then
        Calc(f, EarthModel);  // only recalcs if f changed or rho earth changed

    if assigned(FZreduced) then
        Z := FZReduced
    else
        Z := FZmatrix;

    NewSize := Z.order;
    Result := TCmatrix.CreateMatrix(Newsize);

    Result.CopyFrom(Z);  // gets ohms/meter
    Zvalues := Result.GetValuesArrayPtr(Newsize);  // ptr to the values in the new copy
    // Convert the values by units and length
    UnitLengthConversion := From_per_meter(Units) * lngth;
    for i := 1 to NewSize * NewSize do
        ZValues[i] *= UnitLengthConversion;
end;

procedure TLineConstants.Kron(Norder: Integer);
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

        // Reduce computed matrix one row/col at a time until it is norder

        while Ztemp.order > Norder do
        begin
            FZReduced := Ztemp.Kron(ZTemp.order);    // Eliminate last row

            if not FirstTime then
            begin   // don't throw away original matrix
                Ztemp.Free;  // Ztemp now points to intermediate matrix
            end;
            Ztemp := FZReduced;
            FirstTime := FALSE;
        end;

        // Extract norder x norder portion of Yc matrx
        FYCreduced := TCmatrix.CreateMatrix(Norder);
        for i := 1 to Norder do
            for j := 1 to Norder do
                FYCreduced[i, j] := FYCmatrix[i, j];

        // Left with reduced matrix
    end;
end;

procedure TLineConstants.Reduce();
// Performs a Kron reduction to get rid of neutral conductors
begin
    Kron(nPhases);
end;

procedure TLineConstants.SetCapradius(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then 
        Fcapradius[i] := Value * To_Meters(units);
end;

procedure TLineConstants.SetFrequency(const Value: Double);
begin
    FFrequency := Value;
    Fw := twopi * FFrequency;
    Fme := Csqrt(cmplx(0.0, Fw * Mu0 / FrhoEarth));
end;

procedure TLineConstants.SetRhoEarth(const Value: Double);
begin
    if Value <> FrhoEarth then
        rhoChanged := TRUE;
    FrhoEarth := Value;
    if FFrequency >= 0.0 then
        Fme := Csqrt(cmplx(0.0, Fw * Mu0 / FrhoEarth));
end;

procedure TLineConstants.SetGMR(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
    begin
        FGMR[i] := Value * To_Meters(units);
        if Fradius[i] < 0.0 then
            Fradius[i] := FGMR[i] / 0.7788; // equivalent round conductor
    end;
end;

procedure TLineConstants.SetRac(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FRac[i] := Value * To_per_Meter(units);
end;

procedure TLineConstants.SetRadius(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
    begin
        Fradius[i] := Value * To_Meters(units);
        if FGMR[i] < 0.0 then
            FGMR[i] := Fradius[i] * 0.7788; // Default to round conductor
    end;
end;

procedure TLineConstants.SetRdc(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FRdc[i] := Value * To_per_Meter(units);
end;

procedure TLineConstants.SetX(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FX[i] := Value * To_Meters(units);
end;

procedure TLineConstants.SetY(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= numConductors) then
        FY[i] := Value * To_Meters(units);
end;

function TLineConstants.GetHeightOffset(): Double;
begin
    Result := heightOffset * From_Meters(userHeightUnit);
end;

procedure TLineConstants.SetEpsRMedium(const Value: Double);
begin
    if Value = epsRMedium then
        Exit;

    rhoChanged := true;  // using this for both EpsRMedium, Rho, heightOffset and userHeightUnit
    epsRMedium := Value;
end;

function TLineConstants.GetEpsRMedium(): Double;
begin
    Result := epsRMedium;
end;

procedure TLineConstants.SetHeightOffset(const Value: Double);
var
    NewHeightOffset_m: Double;
    i: Integer;
begin
    NewHeightOffset_m := Value * To_Meters(userHeightUnit);

    if NewHeightOffset_m <> heightOffset then
        rhoChanged := true;  // using this for both EpsRMedium, Rho, heightOffset and userHeightUnit
    // Remove old value from Y positions first
    for i := 1 to numConductors do
    begin
        if (i > 0) and (i <= numConductors) then
            FY[i] -= heightOffset;  // Offset is already in meters
    end;

    heightOffset := NewHeightOffset_m;  // Replace old value with new value

    // Add new value to Y positions
    for i := 1 to numConductors do
    begin
        if (i > 0) and (i <= numConductors) then
            FY[i] += heightOffset;  // Offset is already in meters
    end;
end;


function TLineConstants.GetUserHeightUnit(): Integer;
begin
    Result := userHeightUnit;
end;

procedure TLineConstants.SetUserHeightUnit(const Value: Integer);
begin
    if Value = userHeightUnit then
        Exit;

    userHeightUnit := Value;
    SetHeightOffset(heightOffset);  // This updates the existing value to fit the new user units
end;

procedure TLineConstants.SetEquivalentSpacing(const Value: Boolean);
begin
    if Value = equivalentSpacing then
        Exit;

    equivalentSpacing := Value;
    rhoChanged := true;  // using this for both EpsRMedium, Rho, heightOffset and userHeightUnit and for this one as well.
end;

end.
