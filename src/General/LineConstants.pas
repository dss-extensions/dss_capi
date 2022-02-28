unit LineConstants;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    Arraydef,
    Ucmatrix,
    UComplex, DSSUcomplex,
    LineUnits;

type

    // This class returns a matrix ordered by phases first then remaining conductors
    // Assumes phases are defined first

    TLineConstants = class(TObject)
    PROTECTED
        FNumConds: Integer;
        FNPhases: Integer;

        FData: pDouble;

        // Memory for the arrays below is shared in FData above;

        FX: pDoubleArray;
        FY: pDoubleArray;
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
        FrhoEarth: Double;  // ohm-m
        Fme: Complex; // factor for earth impedance
        FRhoChanged: Boolean;

        function Get_GMR(i, units: Integer): Double;
        function Get_radius(i, units: Integer): Double;
        function Get_Rdc(i, units: Integer): Double;
        function Get_Rac(i, units: Integer): Double;
        function Get_X(i, units: Integer): Double;
        function Get_Y(i, units: Integer): Double;
        function Get_YCmatrix(f, Lngth: Double; Units: Integer): Tcmatrix;
        function Get_Ze(i, j, EarthModel: Integer): Complex;
        function Get_Zint(i, EarthModel: Integer): Complex;
        function Get_Zmatrix(f, Lngth: Double; Units, EarthModel: Integer): Tcmatrix;
        procedure Set_GMR(i, units: Integer; const Value: Double);
        procedure Set_radius(i, units: Integer; const Value: Double);
        procedure Set_Rdc(i, units: Integer; const Value: Double);
        procedure Set_Rac(i, units: Integer; const Value: Double);
        procedure Set_X(i, units: Integer; const Value: Double);
        procedure Set_Y(i, units: Integer; const Value: Double);
        procedure Set_Frequency(const Value: Double);
        procedure Set_Frhoearth(const Value: Double);  // m
        
        // This allows you to compute capacitance using a different radius -- for bundled conductors
        function Get_Capradius(i, units: Integer): Double;
        procedure Set_Capradius(i, units: Integer; const Value: Double);

        // These can only be called privately
        property Frequency: Double READ FFrequency WRITE Set_Frequency;

        procedure set_Nphases(const Value: Integer);

    PUBLIC

        function ConductorsInSameSpace(var ErrorMessage: String): Boolean; VIRTUAL;
        procedure Calc(f: Double; EarthModel: Integer); VIRTUAL; // force a calc of impedances
        procedure Kron(Norder: Integer); VIRTUAL; // Performs a Kron reduction leaving first Norder  rows
        procedure Reduce;  // Kron reduce to Numphases only

        property X[i, units: Integer]: Double READ Get_X WRITE Set_X;
        property Y[i, units: Integer]: Double READ Get_Y WRITE Set_Y;
        property Rdc[i, units: Integer]: Double READ Get_Rdc WRITE Set_Rdc;
        property Rac[i, units: Integer]: Double READ Get_Rac WRITE Set_Rac;
        property radius[i, units: Integer]: Double READ Get_radius WRITE Set_radius;
        Property Capradius[i, units:Integer]:Double Read Get_Capradius Write Set_Capradius;
        property GMR[i, units: Integer]: Double READ Get_GMR WRITE Set_GMR;
        property Zint[i, EarthModel: Integer]: Complex READ Get_Zint;  // Internal impedance of i-th conductor for present frequency
        property Ze[i, j, EarthModel: Integer]: Complex READ Get_Ze;  // Earth return impedance at present frequency for ij element
        property rhoearth: Double READ Frhoearth WRITE Set_Frhoearth;

        // These two properties will auto recalc the impedance matrices if frequency is different
        // Converts to desired units when executed; Returns Pointer to Working Verstion
        property Zmatrix[f, Lngth: Double; Units, EarthModel: Integer]: Tcmatrix READ Get_Zmatrix;
        property YCmatrix[f, Lngth: Double; Units: Integer]: Tcmatrix READ Get_YCmatrix;

        property Nphases: Integer READ FNPhases WRITE set_Nphases;
        property Nconductors: Integer READ FNumConds;

        constructor Create(NumConductors: Integer);
        destructor Destroy; OVERRIDE;

    end;

const
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
    b3: Double = {b1}(1.0 / (3.0 * sqrt(2.0))) / 3.0 / 5.0;
    b4: Double = {b2}(1.0 / 16.0) / 4.0 / 6.0;
    d2: Double = {b2}(1.0 / 16.0) * pi / 4.0;
    d4: Double = {b4}((1.0 / 16.0) / 4.0 / 6.0) * pi / 4.0;
    c2: Double = 1.3659315;
    c4: Double = {c2}(1.3659315) + 1.0 / 4.0 + 1.0 / 6.0;


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
    // RhoEarth := rho;
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

    // For less than 1 kHz use GMR to better match published data

    LFactor := Cmplx(0.0, Fw * mu0 / twopi);
    if (f < 1000.0) and (f > 40.0) then
        PowerFreq := TRUE
    else
        PowerFreq := FALSE;

    // Self Impedances

    for i := 1 to FNumConds do
    begin
        Zi := Get_Zint(i, EarthModel);
        if PowerFreq then
        begin // for less than 1 kHz, use published GMR
            Zi.im := 0.0;
            Zspacing := Lfactor * ln(1.0 / FGMR^[i]);  // use GMR
        end
        else
        begin
            Zspacing := Lfactor * ln(1.0 / Fradius^[i]);
        end;

        FZmatrix.SetElement(i, i, Zi + Zspacing + Get_Ze(i, i, EarthModel));

    end;

    // Mutual IMpedances

    for i := 1 to FNumConds do
    begin
        for j := 1 to i - 1 do
        begin
            Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
            FZmatrix.SetElemSym(i, j, Lfactor * ln(1.0 / Dij) + Get_Ze(i, j, EarthModel));
        end;
    end;

    // Capacitance Matrix

    Pfactor := -1.0 / twopi / e0 / Fw; // include frequency

    // Construct P matrix and then invert

    // Self uses capradius, which defaults to actual conductor radius. But
    // in case of bundled conductors can be specified different in Wiredata.

    for i := 1 to FnumConds do
    begin
        if Fcapradius^[i] < 0 then
            FYCMatrix.SetElement(i, i, cmplx(0.0, pfactor * ln(2.0 * Fy^[i] / Fradius^[i])))
        else
            FYCMatrix.SetElement(i, i, cmplx(0.0, pfactor * ln(2.0 * Fy^[i] / Fcapradius^[i])));
    end;

    for i := 1 to FNumConds do
    begin
        for j := 1 to i - 1 do
        begin
            Dij := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] - Fy^[j]));
            Dijp := sqrt(sqr(Fx^[i] - Fx^[j]) + sqr(Fy^[i] + Fy^[j])); // distance to image j
            FYCMatrix.SetElemSym(i, j, cmplx(0.0, pfactor * ln(Dijp / Dij)));
        end;
    end;

    FYCMatrix.Invert; // now should be nodal C matrix

    if ReducedSize > 0 then
        Kron(ReducedSize);  // Was reduced so reduce again to same size

    // Else the Zmatrix is OK as last computed

    FRhoChanged := FALSE;
end;

function TLineConstants.ConductorsInSameSpace(var ErrorMessage: String): Boolean;
var
    i, j: Integer;
    Dij: Double;
begin
    // Check all conductors to make sure none occupy the same space or are defined at 0,0
    Result := FALSE;

    // Check for 0 Y coordinate
    for i := 1 to FNumConds do
    begin
        if (FY^[i] <= 0.0) then
        begin
            Result := TRUE;
            ErrorMessage := Format('Conductor %d height must be  > 0. ', [i]);
            Exit
        end;
    end;

    // Check for overlapping conductors
    for i := 1 to FNumConds do
    begin
        for j := i + 1 to FNumConds do
        begin
            Dij := Sqrt(SQR(FX^[i] - FX^[j]) + SQR(FY^[i] - FY^[j]));
            if (Dij < (Fradius^[i] + Fradius^[j])) then
            begin
                Result := TRUE;
                ErrorMessage := Format('Conductors %d and %d occupy the same space.',
                    [i, j]);
                Exit;
            end;
        end;
    end;
end;

constructor TLineConstants.Create(NumConductors: Integer);
var
    i: Integer;
begin
    FNumConds := NumConductors;
    NPhases := FNumConds;

    // Data for FX, FY, FGMR, Fradius, Fcapradius, FRdc, FRac, 
    // FZMatrix, FYCMatrix
    FData := Allocmem(Sizeof(Double) * (FNumConds * 7 + FNumconds * FNumconds * (2 * 2)));

    FX := pDoubleArray(FData);
    FY := pDoubleArray(FData + FNumConds);
    FGMR := pDoubleArray(FData + FNumConds * 2);
    Fradius := pDoubleArray(FData + FNumConds * 3);
    Fcapradius := pDoubleArray(FData + FNumConds * 4);
    FRdc := pDoubleArray(FData + FNumConds * 5);
    FRac := pDoubleArray(FData + FNumConds * 6);

    // Initialize to  not set
    for i := 1 to FNumConds do
        FGMR^[i] := -1.0;
    for i := 1 to FNumConds do
        Fradius^[i] := -1.0;
    for i := 1 to FNumConds do
        Fcapradius^[i] := -1.0;
    for i := 1 to FNumConds do
        FRdc^[i] := -1.0;

    FZMatrix := TCMatrix.CreateMatrixInplace(FNumconds, pComplex(FData + FNumConds * 7));
    FYCMatrix := TCMatrix.CreateMatrixInPlace(FNumconds, pComplex(FData + FNumConds * 7 + FNumconds * FNumconds * 2));

    FFrequency := -1.0;  // not computed
    Frhoearth := 100.0;  // default value
    FRhoChanged := TRUE;

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

function TLineConstants.Get_Capradius(i, units: Integer): Double;
begin
    Result := Fcapradius^[i] * From_Meters(Units);
end;

function TLineConstants.Get_GMR(i, units: Integer): Double;
begin
    Result := FGMR^[i] * From_Meters(Units);
end;

function TLineConstants.Get_Rac(i, units: Integer): Double;
begin
    Result := FRAC^[i] * From_per_Meter(Units);
end;

function TLineConstants.Get_radius(i, units: Integer): Double;
begin
    Result := Fradius^[i] * From_Meters(Units);
end;

function TLineConstants.Get_Rdc(i, units: Integer): Double;
begin
    Result := FRDC^[i] * From_per_Meter(Units);
end;

function TLineConstants.Get_X(i, units: Integer): Double;
begin
    Result := FX^[i] * From_Meters(Units);
end;

function TLineConstants.Get_Y(i, units: Integer): Double;
begin
    Result := FY^[i] * From_Meters(Units);
end;

function TLineConstants.Get_YCmatrix(f, Lngth: Double;
    Units: Integer): Tcmatrix;
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
        YCValues^[i] *= UnitLengthConversion;
end;

function TLineConstants.Get_Ze(i, j, EarthModel: Integer): Complex;
var
    LnArg, hterm, xterm: Complex;
    mij, thetaij, Dij, Fyi, Fyj: Double;
    term1, term2, term3, term4, term5: Double;
begin
    Fyi := Abs(Fy^[i]);
    Fyj := Abs(Fy^[j]);

    case EarthModel of

        SIMPLECARSON:
        begin
            Result := cmplx(Fw * Mu0 / 8.0, (Fw * Mu0 / twopi) * ln(658.5 * sqrt(Frhoearth / FFrequency)));
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
                Dij := sqrt(sqr(Fyi + Fyj) + sqr(Fx^[i] - Fx^[j]));
                thetaij := ArcCos((Fyi + Fyj) / Dij);
            end;
            mij := 2.8099e-3 * Dij * sqrt(FFrequency / Frhoearth);

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
                hterm := cmplx(Fyi + Fyj, 0.0) + Cinv(Fme) * 2.0;
                xterm := cmplx(Fx^[i] - Fx^[j], 0.0);
                LnArg := Csqrt(hterm * hterm + xterm * xterm);
                Result := Cmplx(0.0, Fw * Mu0 / twopi) * Cln(lnArg);
            end
            else
            begin
                hterm := cmplx(Fyi, 0.0) + Cinv(Fme);
                Result := Cmplx(0.0, Fw * Mu0 / twopi) * Cln(hterm * 2.0);
            end;
 // {****}          WriteDLLDebugFile(Format('Deri: Z(%d,%d) = %.8g +j %.8g; hterm= %.8g + j %.8g',[i,j, Result.re, result.im, hterm.re, hterm.im]));
        end;
    end;
end;

function TLineConstants.Get_Zint(i, EarthModel: Integer): Complex;
var
    Alpha, I0I1: Complex;
begin
    case EarthModel of
        SIMPLECARSON:
        begin
            Result := cmplx(FRac^[i], Fw * Mu0 / (8 * pi));
        end;
        FULLCARSON:
        begin // no skin effect
            Result := cmplx(FRac^[i], Fw * Mu0 / (8 * pi));
        end;
        DERI:
        begin // with skin effect model
            // Assume round conductor
            Alpha := c1_j1 * sqrt(FFrequency * mu0 / FRDC^[i]);
            if Cabs(Alpha) > 35.0 then
                I0I1 := CONE
            else
                I0I1 := Bessel_I0(Alpha) / Bessel_I1(Alpha);

            Result := C1_j1 * I0I1 * (Sqrt(FRdc^[i] * FFrequency * mu0) / 2.0);
        end;
    end;
end;

function TLineConstants.Get_Zmatrix(f, Lngth: Double;
    Units, EarthModel: Integer): Tcmatrix;
// Makes a new Zmatrix and correct for lengths and units as it copies
// Uses the reduced Zmatrix by default if it exists
var
    Newsize, i: Integer;
    UnitLengthConversion: Double;
    Z: TCMatrix;
    ZValues: pComplexArray;
begin
    if (F <> FFrequency) or FRhoChanged then
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
        ZValues^[i] *= UnitLengthConversion;
end;

procedure TLineConstants.Kron(Norder: Integer);
var
    Ztemp: TCmatrix;
    FirstTime: Boolean;
    i, j: Integer;
begin
    Ztemp := FZMatrix;
    FirstTime := TRUE;

    if (FFrequency >= 0.0) and (Norder > 0) and (Norder < FnumConds) then
    begin
        if Assigned(FZreduced) then
            FZreduced.Free;
        if Assigned(FYCreduced) then
            FYCReduced.Free;

        // Reduce computed matrix one row/col at a time until it is norder

        while Ztemp.Order > Norder do
        begin
            FZReduced := Ztemp.Kron(ZTemp.Order);    // Eliminate last row

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
                FYCreduced.SetElement(i, j, FYCmatrix.GetElement(i, j));

        // Left with reduced matrix
    end;
end;

procedure TLineConstants.Reduce;
// Performs a Kron reduction to get rid of neutral conductors
begin
    Kron(FNPhases);
end;

procedure TLineConstants.Set_Capradius(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then 
        Fcapradius^[i] := Value * To_Meters(units);
end;

procedure TLineConstants.Set_Frequency(const Value: Double);
begin
    FFrequency := Value;
    Fw := twopi * FFrequency;
    Fme := Csqrt(cmplx(0.0, Fw * Mu0 / Frhoearth));
end;

procedure TLineConstants.Set_Frhoearth(const Value: Double);
begin
    if Value <> Frhoearth then
        FRhoChanged := TRUE;
    Frhoearth := Value;
    if FFrequency >= 0.0 then
        Fme := Csqrt(cmplx(0.0, Fw * Mu0 / Frhoearth));
end;

procedure TLineConstants.Set_GMR(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
    begin
        FGMR^[i] := Value * To_Meters(units);
        if Fradius^[i] < 0.0 then
            Fradius^[i] := FGMR^[i] / 0.7788; // equivalent round conductor
    end;
end;

procedure TLineConstants.set_Nphases(const Value: Integer);
begin
    FNPhases := Value;
end;

procedure TLineConstants.Set_Rac(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FRac^[i] := Value * To_per_Meter(units);
end;

procedure TLineConstants.Set_radius(i, units: Integer;
    const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
    begin
        Fradius^[i] := Value * To_Meters(units);
        if FGMR^[i] < 0.0 then
            FGMR^[i] := Fradius^[i] * 0.7788; // Default to round conductor
    end;
end;

procedure TLineConstants.Set_Rdc(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FRdc^[i] := Value * To_per_Meter(units);
end;

procedure TLineConstants.Set_X(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FX^[i] := Value * To_Meters(units);
end;

procedure TLineConstants.Set_Y(i, units: Integer; const Value: Double);
begin
    if (i > 0) and (i <= FNumConds) then
        FY^[i] := Value * To_Meters(units);
end;

end.
