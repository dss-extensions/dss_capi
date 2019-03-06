unit Mathutil;

   {Math utilities}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Arraydef,
    uComplex;

procedure AB02Phase(Vph, VaB0: pComplexArray);     // Reverse Clarke
function Bessel_I0(const a: Complex): Complex;
function Bessel_I1(const x: Complex): Complex;
procedure CalcKPowers(kWkvar, V, I: pComplexArray; N: Integer);
procedure ETKInvert(A: pDoubleArray; Norder: Integer; var Error: Integer);  // Real Matrix Inversion
function Gauss(Mean, StdDev: Double): Double;
function GetXR(const A: Complex): Double;
function ParallelZ(const Z1, Z2: Complex): Complex;
procedure Phase2AB0(Vph, VaB0: pComplexArray);     // Forward Clarke
procedure Phase2SymComp(Vph, V012: pComplexArray);
function QuasiLogNormal(Mean: Double): Double;
procedure RCDMeanAndStdDev(pData: Pointer; Ndata: Integer; var Mean, StdDev: Double);
procedure CurveMeanAndStdDev(pY: pDoubleArray; pX: pDoubleArray; N: Integer; var Mean, StdDev: Double);
//         function  RCDSum( Data:Pointer; Count:Integer): Extended; register;
procedure SymComp2Phase(Vph, V012: pComplexArray);
function TerminalPowerIn(V, I: pComplexArray; Nphases: Integer): Complex;
function PctNemaUnbalance(Vph: pComplexArray): Double;
procedure DblInc(var x: Double; const y: Double); inline; // increment a double

implementation

uses
    uCmatrix,
    Math;

var
    As2p, Ap2s, ClarkeF, ClarkeR: TcMatrix;
{Symmetrical Component Conversion Matrices}
   // Sqrt23:Double;

procedure ETKInvert(A: pDoubleArray; Norder: Integer; var Error: Integer);

{
    Matrix= reference to matrix of DOUBLEs
        Norder=  order of matrix  (assumed square)
        Error     = 0 if no error;
            = 1 if not enough heap to alloc temp array
                = 2 if matrix can't be inverted

        This routine will invert a non-symmetric matrix.  Index is assumed to
        follow the FORTRAN standard, not the Pascal standard.  That is the data
        are ordered by first subscript first, then second subscript.  This routine
        computes its own indexing, leaving nothing to the whims of a cantankerous compiler.

        It assumes that the matrix is dimensioned to exactly the number of elements
        needed.  Apologies to Fortran users who are accustomed to over dimensioning
        stuff.

}

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

{Allocate LT}
    LT := NIL;
    Reallocmem(LT, SizeOf(LT^[1]) * L);
    if LT = NIL then
    begin
        Error := 1;
        Exit;
    end;

{Zero LT}
    for j := 1 to L do
        LT^[j] := 0;

    T1 := 0.0;

{M Loop }
    // initialize a safe value of k
    k := 1;

    for  M := 1 to L do
    begin
        for  LL := 1 to L do
        begin
            if LT^[LL] <> 1 then
            begin
                RMY := Abs(A^[Index(LL, LL)]) - Abs(T1);
                if RMY > 0.0 then
                begin
                    T1 := A^[Index(LL, LL)];
                    K := LL;
                end; {RMY}
            end; {IF LT}
        end; {LL}

{Error Check.  If RMY ends up zero, matrix is non-inversible}
        RMY := Abs(T1);
        if RMY = 0.0 then
        begin
            Error := 2;
            Exit;
        end;

        T1 := 0.0;
        LT^[k] := 1;
        for i := 1 to L do
            if i <> k then
                for j := 1 to L do
                    if j <> k then
                        A^[Index(i, j)] :=
                            A^[Index(i, j)] - A^[Index(i, k)] * A^[Index(k, j)] / A^[Index(k, k)];

        A^[Index(k, k)] := -1.0 / A^[Index(k, k)];

        for  i := 1 to L do
            if i <> k then
            begin
                A^[Index(i, k)] := A^[Index(i, k)] * A^[Index(k, k)];
                A^[Index(k, i)] := A^[Index(k, i)] * A^[Index(k, k)];
            end;  {if}

    end; {M loop}

    for  j := 1 to L do
        for  k := 1 to L do
            A^[Index(j, k)] := -A^[Index(j, k)];

    Reallocmem(LT, 0);  {Dispose of LT}

end; {Proc Invert}


{-------------------------------------------------------------}
procedure Phase2SymComp(Vph, V012: pComplexArray);
begin
    with Ap2s do
    begin
        MvMult(V012, Vph);
    end;

end;

{-------------------------------------------------------------}
procedure SymComp2Phase(Vph, V012: pComplexArray);
begin
    with As2p do
    begin
        MvMult(Vph, V012);
    end;

end;

{-------------------------------------------------------------}
procedure SetClarkeMatrices;
var
    Sin2pi3: Double;

begin

    Sin2pi3 := Sin(2.0 * PI / 3.0);
    with ClarkeF do
    begin       // Forward Clarke
        SetElement(1, 1, cmplx(1.0, 0.0));
        SetElement(1, 2, cmplx(-0.5, 0.0));
        SetElement(1, 3, cmplx(-0.5, 0.0));

        SetElement(2, 2, cmplx(Sin2pi3, 0.0));
        SetElement(2, 3, cmplx(-Sin2pi3, 0.0));

        SetElement(3, 1, Cmplx(0.5, 0.0));
        SetElement(3, 2, Cmplx(0.5, 0.0));
        SetElement(3, 3, Cmplx(0.5, 0.0));

        MultByConst(2.0 / 3.0);  // multiply all elements by a const  2/3
    end;

    with ClarkeR do
    begin       // Reverse Clarke
        SetElement(1, 1, cmplx(1.0, 0.0));
        SetElement(2, 1, cmplx(-0.5, 0.0));
        SetElement(3, 1, cmplx(-0.5, 0.0));

        SetElement(2, 2, cmplx(Sin2pi3, 0.0));
        SetElement(3, 2, cmplx(-Sin2pi3, 0.0));

        SetElement(1, 3, Cmplx(1.0, 0.0));
        SetElement(2, 3, Cmplx(1.0, 0.0));
        SetElement(3, 3, Cmplx(1.0, 0.0));

    end;

end;

{-------------------------------------------------------------}

procedure Phase2AB0(Vph, VaB0: pComplexArray);     // Forward Clarke

begin
    with ClarkeF do
    begin
        MvMult(VaB0, Vph);
    end;
end;


{-------------------------------------------------------------}
procedure AB02Phase(Vph, VaB0: pComplexArray);     // Reverse Clarke

begin
    with ClarkeR do
    begin
        MvMult(Vph, VaB0);
    end;
end;


function TerminalPowerIn(V, I: pComplexArray; Nphases: Integer): Complex;
// Computes total complex power given terminal  voltages and currents

var
    j: Integer;

begin
    Result := CZERO;
    for j := 1 to Nphases do
    begin
        Caccum(Result, Cmul(V^[j], Conjg(I^[j])));
    end;

end;

{-------------------------------------------------------------}
procedure CalcKPowers(kWkvar, V, I: pComplexArray; N: Integer);

{Compute complex power in kW and kvar in each phase}

var
    j: Integer;
begin

    for j := 1 to N do
    begin
        kWkVAR^[j] := CMulReal(Cmul(V^[j], Conjg(I^[j])), 0.001);
    end;

end;

{-------------------------------------------------------------}
procedure SetAMatrix(Amat: Tcmatrix);
var
    a, aa: complex;
    i: Integer;
begin
    a := cmplx(-0.5, 0.8660254037844387);
    aa := cmplx(-0.5, -0.8660254037844387);
    with Amat do
    begin
        for i := 1 to 3 do
            SetElemSym(1, i, CONE);
        SetElement(2, 2, aa);
        SetElement(3, 3, aa);
        SetElemsym(2, 3, a);
    end;

end;


{-------------------------------------------------------------}
procedure SetAMatrix_inv(Amat_inv: Tcmatrix);
var
    a_3, aa_3, one_3: complex;
    i: Integer;
begin
    a_3 := CDiv(cmplx(-0.5, 0.8660254037844387), cmplx(3, 0));
    aa_3 := CDiv(cmplx(-0.5, -0.8660254037844387), cmplx(3, 0));
    one_3 := CDiv(CONE, cmplx(3, 0));
    with Amat_inv do
    begin
        for i := 1 to 3 do
            SetElemSym(1, i, one_3);
        SetElement(2, 2, a_3);
        SetElement(3, 3, a_3);
        SetElemsym(2, 3, aa_3);
    end;

end;


{-------------------------------------------------------------}
function Gauss(Mean, StdDev: Double): Double;
{Returns a normally distributed random variable}
var
    i: Integer;
    A: Double;
begin
    A := 0.0;
    for i := 1 to 12 do
        A := A + Random;
    Result := (A - 6.0) * StdDev + Mean;
end;

{-------------------------------------------------------------}
function QuasiLogNormal(Mean: Double): Double;

{Generates a quasi-lognormal distribution with approx 50% of values from 0 to Mean and the remainder from Mean to infinity}
begin

    Result := exp(Gauss(0.0, 1.0)) * Mean;

end;

//- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

{$IF NOT (Defined(CPUX64) or Defined(Darwin) or Defined(Unix))}
function RCDSUM(Data: Pointer; Count: Integer): Extended; REGISTER;

// Sums an array of doubles quickly

{ With register convention first 3 parameters are passed EAX, EDX, ECX and
  remainder on stack}

asm  // IN: EAX = ptr to Data, EDX = High(Data) = Count - 1
     // Uses 4 accumulators to minimize read-after-write delays and loop overhead
     // 5 clocks per loop, 4 items per loop = 1.2 clocks per item
    FLDZ
    SUB      EDX, 1    // now EDX contains Count - 1
    MOV      ECX, EDX
    FLD      ST(0)
    AND      EDX, not 3
    FLD      ST(0)
    AND      ECX, 3
    FLD      ST(0)
    SHL      EDX, 3      // count * sizeof(Double) = count * 8
    JMP      @Vector.Pointer[ECX*4]
    @Vector:
    DD @@1
    DD @@2
    DD @@3
    DD @@4
    @@4:   FADD     qword ptr [EAX+EDX+24]    // 1
    FXCH     ST(3)                     // 0
    @@3:   FADD     qword ptr [EAX+EDX+16]    // 1
    FXCH     ST(2)                     // 0
    @@2:   FADD     qword ptr [EAX+EDX+8]     // 1
    FXCH     ST(1)                     // 0
    @@1:   FADD     qword ptr [EAX+EDX]       // 1
    FXCH     ST(2)                     // 0
    SUB      EDX, 32
    JNS      @@4
    FADDP    ST(3),ST                  // ST(3) := ST + ST(3); Pop ST
    FADD                               // ST(1) := ST + ST(1); Pop ST
    FADD                               // ST(1) := ST + ST(1); Pop ST
    FWAIT
end;
{$ENDIF}

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
        Mean := Data^[1];
        StdDev := Data^[1];
        Exit;
    end;
{$IF (Defined(CPUX64) or Defined(Darwin) or Defined(Unix))}
    Mean := 0.0;
    for i := 1 to NData do
        Mean := Mean + Data^[i];
    Mean := Mean / Ndata;
{$ELSE ! CPUX86}
    Mean := RCDSum(Data, (Ndata)) / Ndata;
{$ENDIF}
    S := 0;               // sum differences from the mean, for greater accuracy
    for i := 1 to Ndata do
        S := S + Sqr(Mean - Data^[i]);
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
    {Parallel two complex impedances}
    Denom := Cadd(Z1, Z2);
    if (Abs(Denom.Re) > 0.0) or (abs(Denom.im) > 0.0) then
        Result := CDiv(Cmul(Z1, Z2), Denom)
    else {Error}
        Result := CZERO;
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
    RESULT := COne;                // term 0
    zSQR25 := CmulReal(Cmul(a, a), 0.25);
    term := zSQR25;
    CAccum(RESULT, zSQR25);      // term 1
    i := 1;
    repeat
        term := CMul(zSQR25, term);
        INC(i);
        Term := CDivReal(term, SQR(i));
        CAccum(RESULT, term);          // sum := sum + term
        SizeSqr := SQR(term.re) + SQR(term.im)
    until (i > MaxTerm) or (SizeSqr < EpsilonSqr)
end {Bessel_I0};

function Bessel_I1(const x: Complex): Complex;
const
    MaxTerm: Integer = 1000;
    EpsilonSqr: Double = 1.0E-20;

var
    i: Integer;
    term, incterm, newterm: Complex;
    SizeSqr: Double;

begin
    term := CdivReal(x, 2);
    Result := Term;
    incTerm := Term;
    i := 4;
    repeat
        newterm := CdivReal(x, i);
        Term := Cmul(term, cmul(incterm, newterm));
        Caccum(Result, Term);
        incterm := newterm;
        inc(i, 2);
        SizeSqr := SQR(term.re) + SQR(term.im)
    until (i > MaxTerm) or (SizeSqr < EpsilonSqr)

end;

function PctNemaUnbalance(Vph: pComplexArray): Double;

{Return Nema unbalance }
var
    i: Integer;
    Vavg: Double;
    MaxDiff: Double;
    VMag: array[1..3] of Double;

begin
    for i := 1 to 3 do
        VMag[i] := cabs(Vph^[i]);

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

procedure DblInc(var x: Double; const y: Double); inline;

begin
    x := x + y;
end;

initialization
    Randomize;
    As2p := TcMatrix.CreateMatrix(3);
    Ap2s := TcMatrix.CreateMatrix(3);
    ClarkeF := TcMatrix.CreateMatrix(3);
    ClarkeR := TcMatrix.CreateMatrix(3);
    SetAMatrix(As2p);
    SetAMatrix_inv(Ap2s);
    SetClarkeMatrices;
    // Sqrt23 := Sqrt(2.0/3.0); // for park
finalization
    As2p.Free;
    Ap2s.Free;
    ClarkeF.Free;
    ClarkeR.Free;
end.
