unit Mathutil;

// Math utilities

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

uses
    Arraydef,
    UComplex, DSSUcomplex,
    uCmatrix;

type 
    Complex3 = Array[1..3] of Complex;
    PComplex3 = ^Complex3;

    TPICtrl = Class(TObject)
    private
        den, num: Array[0..1] of Double;
    public
        kNum: Double;
        kDen: Double;
        Kp: Double;

        function SolvePI(SetPoint : Double):Double;
        constructor Create;
        destructor Destroy; override;
    end;

function Bessel_I0(const a: Complex): Complex;
function Bessel_I1(const x: Complex): Complex;
procedure CalcKPowers(kWkvar, V, I: pComplexArray; N: Integer);
procedure ETKInvert(A: pDoubleArray; Norder: Integer; var Error: Integer);  // Real Matrix Inversion
function Gauss(Mean, StdDev: Double): Double;
function GetXR(const A: Complex): Double;
function ParallelZ(const Z1, Z2: Complex): Complex;
procedure Phase2SymComp(Vph, V012: PComplex3); overload;
procedure Phase2SymComp(Vph, V012: pComplexArray); overload;
function QuasiLogNormal(Mean: Double): Double;
procedure RCDMeanAndStdDev(pData: Pointer; Ndata: Integer; var Mean, StdDev: Double);
procedure RCDMeanAndStdDevSingle(pData: Pointer; Ndata: Integer; var Mean, StdDev: Double);
procedure CurveMeanAndStdDev(pY: pDoubleArray; pX: pDoubleArray; N: Integer; var Mean, StdDev: Double);
procedure CurveMeanAndStdDevSingle(pY: pSingleArray; pX: pSingleArray; N: Integer; var Mean, StdDev: Double);

procedure SymComp2Phase(Vph, V012: pComplexArray);
procedure SymComp2Phase(Vph, V012: PComplex3);
function TerminalPowerIn(V, I: pComplexArray; Nphases: Integer): Complex;
function PctNemaUnbalance(Vph: PComplex3): Double;
procedure SelectAs2pVersion(useOfficial: Boolean);

var
   As2p, Ap2s: TcMatrix; // Symmetrical Component Conversion Matrices

implementation

uses
    Math;

var
   As2p_official, Ap2s_official: TcMatrix; // Symmetrical Component Conversion Matrices, usptream versions
   As2p_ours, Ap2s_ours: TcMatrix; // Symmetrical Component Conversion Matrices, better precision

constructor TPICtrl.Create;
begin
    //Initializes the constants for a rising function of 5 steps
    kNum := 0.8647;
    kDen := 0.1353;
    Kp := 0.02;
    den[1] := 0;
    num[1] := 0;
end;

destructor TPICtrl.Destroy;
begin
end;

function TPICtrl.SolvePI(SetPoint: Double): Double;
Begin
    num[0] := num[1];
    num[1] := SetPoint * Kp;
    den[0] := den[1];
    den[1] := (num[0] * kNum) + (den[0] * kDen);
    Result := den[1];
End;


procedure ETKInvert(A: pDoubleArray; Norder: Integer; var Error: Integer);
//    Matrix= reference to matrix of DOUBLEs
//        Norder=  order of matrix  (assumed square)
//        Error     = 0 if no error;
//            = 1 if not enough heap to alloc temp array
//                = 2 if matrix can't be inverted
//
//        This routine will invert a non-symmetric matrix.  Index is assumed to
//        follow the FORTRAN standard, not the Pascal standard.  That is the data
//        are ordered by first subscript first, then second subscript.  This routine
//        computes its own indexing, leaving nothing to the whims of a cantankerous compiler.
//
//        It assumes that the matrix is dimensioned to exactly the number of elements
//        needed.  Apologies to Fortran users who are accustomed to over dimensioning
//        stuff.
var
    j, k, L, LL, M, i: Integer;
    LT: pIntegerArray;
    RMY, T1: Double;

    function Index(i, j: Integer): Integer;
    begin
        Index := (j - 1) * L + i;
    end;
begin
    L := Norder;
    Error := 0;

    // Allocate LT
    LT := NIL;
    Reallocmem(LT, SizeOf(LT[1]) * L);
    if LT = NIL then
    begin
        Error := 1;
        Exit;
    end;

    // Zero LT
    for j := 1 to L do
        LT[j] := 0;

    T1 := 0.0;

    // M Loop
    // initialize a safe value of k
    k := 1;

    for  M := 1 to L do
    begin
        for  LL := 1 to L do
        begin
            if LT[LL] <> 1 then
            begin
                RMY := Abs(A[Index(LL, LL)]) - Abs(T1);
                if RMY > 0.0 then
                begin
                    T1 := A[Index(LL, LL)];
                    K := LL;
                end;
            end;
        end;

        // Error Check.  If RMY ends up zero, matrix is non-inversible
        RMY := Abs(T1);
        if RMY = 0.0 then
        begin
            Error := 2;
            Exit;
        end;

        T1 := 0.0;
        LT[k] := 1;
        for i := 1 to L do
            if i <> k then
                for j := 1 to L do
                    if j <> k then
                        A[Index(i, j)] :=
                            A[Index(i, j)] - A[Index(i, k)] * A[Index(k, j)] / A[Index(k, k)];

        A[Index(k, k)] := -1.0 / A[Index(k, k)];

        for  i := 1 to L do
            if i <> k then
            begin
                A[Index(i, k)] := A[Index(i, k)] * A[Index(k, k)];
                A[Index(k, i)] := A[Index(k, i)] * A[Index(k, k)];
            end;

    end; // M loop

    for  j := 1 to L do
        for  k := 1 to L do
            A[Index(j, k)] := -A[Index(j, k)];

    Reallocmem(LT, 0);
end;

procedure Phase2SymComp(Vph, V012: PComplex3);
begin
    Ap2s.MvMult(PComplexArray(V012), PComplexArray(Vph)); // TODO: dedicated/optimized version?
end;
procedure Phase2SymComp(Vph, V012: pComplexArray);
begin
    Ap2s.MvMult(V012, Vph);
end;

procedure SymComp2Phase(Vph, V012: PComplex3);
begin
    As2p.MvMult(PComplexArray(Vph), PComplexArray(V012)); // TODO: dedicated/optimized version?
end;

procedure SymComp2Phase(Vph, V012: pComplexArray);
begin
    As2p.MvMult(Vph, V012);
end;

function TerminalPowerIn(V, I: pComplexArray; Nphases: Integer): Complex;
// Computes total complex power given terminal  voltages and currents
var
    j: Integer;
begin
    Result := 0;
    for j := 1 to Nphases do
    begin
        Result += V[j] * cong(I[j]);
    end;
end;

procedure CalcKPowers(kWkvar, V, I: pComplexArray; N: Integer);
// Compute complex power in kW and kvar in each phase
var
    j: Integer;
begin
    for j := 1 to N do
    begin
        kWkVAR[j] := V[j] * cong(I[j]) * 0.001;
    end;
end;

procedure SetAMatrix(Amat: Tcmatrix);
var
    a, aa: complex;
begin
    a := cmplx(-0.5, 0.8660254037844387);
    aa := cmplx(-0.5, -0.8660254037844387);

    Amat[1, 1] := 1;
    Amat[1, 2] := 1;
    Amat[1, 3] := 1;

    Amat[2, 1] := 1;
    Amat[2, 2] := aa;
    Amat[2, 3] := a;

    Amat[3, 1] := 1;
    Amat[3, 2] := a;
    Amat[3, 3] := aa;
end;

procedure SetAMatrix_inv(Amat_inv: Tcmatrix);
var
    a_3, aa_3, one_3: complex;
begin
    a_3 := cmplx(-0.5, 0.8660254037844387) / 3;
    aa_3 := cmplx(-0.5, -0.8660254037844387) / 3;
    one_3 := 1.0 / 3;

    Amat_inv[1, 1] := one_3;
    Amat_inv[1, 2] := one_3;
    Amat_inv[1, 3] := one_3;

    Amat_inv[2, 1] := one_3;
    Amat_inv[2, 2] := a_3;
    Amat_inv[2, 3] := aa_3;

    Amat_inv[3, 1] := one_3;
    Amat_inv[3, 2] := aa_3;
    Amat_inv[3, 3] := a_3;
end;

procedure SetAMatrix_official(Amat: Tcmatrix);
var
    a, aa: complex;
begin
    a := cmplx(-0.5,0.866025403);
    aa := cmplx(-0.5,-0.866025403);    

    Amat[1, 1] := 1;
    Amat[1, 2] := 1;
    Amat[1, 3] := 1;

    Amat[2, 1] := 1;
    Amat[2, 2] := aa;
    Amat[2, 3] := a;

    Amat[3, 1] := 1;
    Amat[3, 2] := a;
    Amat[3, 3] := aa;
end;


function Gauss(Mean, StdDev: Double): Double;
// Returns a normally distributed random variable
var
    i: Integer;
    A: Double;
begin
    A := 0.0;
    for i := 1 to 12 do
        A := A + Random;
    Result := (A - 6.0) * StdDev + Mean;
end;

function QuasiLogNormal(Mean: Double): Double;
// Generates a quasi-lognormal distribution with approx 50% of values from 0 to Mean and the remainder from Mean to infinity
begin
    Result := exp(Gauss(0.0, 1.0)) * Mean;
end;

procedure RCDMeanAndStdDev(pData: Pointer; Ndata: Integer; var Mean, StdDev: Double);
type
    pDoubleArray = ^DoubleArray;
    DoubleArray = array[1..100] of Double;
var
    Data: pDoubleArray;
    S: Double;
    i: Integer;
begin
    Data := pData;  // make a double pointer
    if Ndata = 1 then
    begin
        Mean := Data[1];
        StdDev := Data[1];
        Exit;
    end;
    Mean := 0.0;
    for i := 1 to NData do
        Mean := Mean + Data[i];
    Mean := Mean / Ndata;
    S := 0;               // sum differences from the mean, for greater accuracy
    for i := 1 to Ndata do
        S := S + Sqr(Mean - Data[i]);
    StdDev := Sqrt(S / (Ndata - 1));
end;

procedure RCDMeanAndStdDevSingle(pData: Pointer; Ndata: Integer; var Mean, StdDev: Double);
type
    SingleArray = array[1..100] of Single;
    pSingleArray = ^SingleArray;
var
    Data: pSingleArray;
    S: Single;
    i: Integer;
begin
    Data := pData;
    if Ndata = 1 then
    begin
        Mean := Data[1];
        StdDev := Data[1];
        Exit;
    end;
    Mean := 0.0;
    for i := 1 to NData do
        Mean := Mean + Data[i];
    Mean := Mean / Ndata;
    S := 0;               // sum differences from the mean, for greater accuracy
    for i := 1 to Ndata do
        S := S + Sqr(Mean - Data[i]);
    StdDev := Sqrt(S / (Ndata - 1));
end;

procedure CurveMeanAndStdDev(pY: pDoubleArray; pX: pDoubleArray; N: Integer; var Mean, StdDev: Double);
var
    s, dy1, dy2: Double;
    i: Integer;
begin
    if N = 1 then
    begin
        Mean := pY[1];
        StdDev := pY[1];
        Exit;
    end;
    s := 0;
    for i := 1 to N - 1 do
    begin
        s := s + 0.5 * (pY[i] + pY[i + 1]) * (pX[i + 1] - pX[i]);
    end;
    Mean := s / (pX[N] - pX[1]);

    S := 0;               // sum differences from the mean, for greater accuracy
    for i := 1 to N - 1 do
    begin
        dy1 := (pY[i] - Mean);
        dy2 := (pY[i + 1] - Mean);
        s := s + 0.5 * (dy1 * dy1 + dy2 * dy2) * (pX[i + 1] - pX[i]);
    end;
    StdDev := Sqrt(s / (pX[N] - pX[1]));
end;

procedure CurveMeanAndStdDevSingle(pY: pSingleArray; pX: pSingleArray; N: Integer; var Mean, StdDev: Double);
var
    s, dy1, dy2: Double;
    i: Integer;
begin
    if N = 1 then
    begin
        Mean := pY[1];
        StdDev := pY[1];
        Exit;
    end;
    s := 0;
    for i := 1 to N - 1 do
    begin
        s := s + 0.5 * (pY[i] + pY[i + 1]) * (pX[i + 1] - pX[i]);
    end;
    Mean := s / (pX[N] - pX[1]);

    S := 0;               // sum differences from the mean, for greater accuracy
    for i := 1 to N - 1 do
    begin
        dy1 := (pY[i] - Mean);
        dy2 := (pY[i + 1] - Mean);
        s := s + 0.5 * (dy1 * dy1 + dy2 * dy2) * (pX[i + 1] - pX[i]);
    end;
    StdDev := Sqrt(s / (pX[N] - pX[1]));
end;

function GetXR(const A: Complex): Double;
begin
    if A.re <> 0.0 then
    begin
        Result := A.im / A.re;
        if Abs(Result) > 9999.0 then
            Result := 9999.0;
    end
    else
        Result := 9999.0;
    ;
end;

function ParallelZ(const Z1, Z2: Complex): Complex;
var
    Denom: Complex;
begin
    // Parallel two complex impedances
    Denom := Z1 + Z2;
    if (Abs(Denom.Re) > 0.0) or (abs(Denom.im) > 0.0) then
        Result := (Z1 * Z2) / Denom
    else // Error
        Result := 0;
end;

// z = I0(a)
function Bessel_I0(const a: Complex): Complex;
const
    MaxTerm: Integer = 1000;
    EpsilonSqr: Double = 1.0E-20;
var
    i: Integer;
    SizeSqr: Double;
    term: Complex;
    zSQR25: Complex;

begin
    RESULT := 1;                // term 0
    zSQR25 := (a * a) * 0.25;
    term := zSQR25;
    RESULT += zSQR25;      // term 1
    i := 1;
    repeat
        term := zSQR25 * term;
        INC(i);
        Term := term / SQR(i);
        RESULT += term;          // sum := sum + term
        SizeSqr := SQR(term.re) + SQR(term.im)
    until (i > MaxTerm) or (SizeSqr < EpsilonSqr)
end;

function Bessel_I1(const x: Complex): Complex;
const
    MaxTerm: Integer = 1000;
    EpsilonSqr: Double = 1.0E-20;
var
    i: Integer;
    term, incterm, newterm: Complex;
    SizeSqr: Double;
begin
    term := x / 2;
    Result := Term;
    incTerm := Term;
    i := 4;
    repeat
        newterm := x / i;
        Term := Term * (incterm * newterm);
        Result += Term;
        incterm := newterm;
        inc(i, 2);
        SizeSqr := SQR(term.re) + SQR(term.im)
    until (i > MaxTerm) or (SizeSqr < EpsilonSqr)
end;

function PctNemaUnbalance(Vph: PComplex3): Double;
// Return Nema unbalance
var
    i: Integer;
    Vavg: Double;
    MaxDiff: Double;
    VMag: array[1..3] of Double;
begin
    for i := 1 to 3 do
        VMag[i] := cabs(Vph[i]);

    Vavg := 0.0;
    for i := 1 to 3 do
        Vavg := Vavg + VMag[i];
    Vavg := Vavg / 3.0;

    MaxDiff := 0.0;
    for i := 1 to 3 do
        MaxDiff := Max(MaxDiff, abs(Vmag[i] - Vavg));

    if Vavg <> 0.0 then
        Result := MaxDiff / Vavg * 100.0  // pct difference
    else
        Result := 0.0;
end;

procedure SelectAs2pVersion(useOfficial: Boolean);
begin
    if useOfficial then
    begin
        // WriteLn('SelectAs2pVersion: selecting worse precision, but numerically compatible with official OpenDSS');
        Ap2s := Ap2s_official;
        As2p := As2p_official;
    end
    else
    begin
        // WriteLn('SelectAs2pVersion: selecting beter precision, but numerically incompatible with official OpenDSS');
        Ap2s := Ap2s_ours;
        As2p := As2p_ours;
    end;
end;

initialization
    Randomize;
    As2p_ours := TcMatrix.CreateMatrix(3);
    Ap2s_ours := TcMatrix.CreateMatrix(3);
    SetAMatrix(As2p_ours);
    SetAMatrix_inv(Ap2s_ours);

    As2p_official := TcMatrix.CreateMatrix(3);
    Ap2s_official := TcMatrix.CreateMatrix(3);
    SetAMatrix_official(As2p_official);
    SetAMatrix_official(Ap2s_official);
    Ap2s_official.Invert();

    // select ours by default
    SelectAs2pVersion(False);

finalization
    As2p_official.Free;
    Ap2s_official.Free;
    As2p_ours.Free;
    Ap2s_ours.Free;
end.

