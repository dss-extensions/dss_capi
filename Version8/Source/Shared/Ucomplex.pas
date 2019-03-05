unit Ucomplex;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

type
    pcomplex = ^complex;

    complex = record
        re, im: Double;
    end;
    pComplexArray = ^ComplexArray;
    ComplexArray = array [1..100] of Complex;

    polar = record
        mag, ang: Double;
    end;

  // 4-8-2010  added inlining selected often-used functions
function cmplx(const a, b: Double): complex; inline;
function cinv(const A: COMPLEX): COMPLEX; inline;
function cabs(const a: complex): Double; inline;
function cang(const a: complex): Double;
function cdang(const a: complex): Double; // angle of complex number, degrees
function ctopolar(const a: complex): polar;
function ctopolardeg(const a: complex): polar;  // complex to polar, degrees
function cadd(const a, b: complex): complex; inline;
procedure caccum(var a: complex; const b: complex); inline; {a := a + b}
function csub(const a, b: complex): complex; inline;
function cmul(const a, b: complex): complex; inline;
procedure caccumarray(a, b: pComplexArray; N: Smallint);
function cmulreal(const a: complex; const b: Double): Complex; inline; { := a*b }
procedure cmulrealaccum(var a: complex; const b: Double); inline; { a=a*b}
function cdiv(const a, b: complex): complex; inline;
function cdivreal(const a: complex; const b: Double): Complex; inline; { := a /b}
function conjg(const a: complex): complex; inline;
function cnegate(const a: complex): complex; inline;
function csqrt(const a: complex): complex;
function cln(const a: complex): complex;
function topolar(const a, b: Double): polar; inline;  // scalar to polar
function prel(const a: polar): Double;  // real part of polar number   |a| cos()
function pimg(const a: polar): Double;  // imag part of polar number   |a| sin()
function ptocomplex(const a: polar): complex;
function padd(const a, b: polar): polar;
function psub(const a, b: polar): polar;
function pmul(const a, b: polar): polar;
function pdiv(const a, b: polar): polar;
function pdegtocomplex(const magn, angle: Double): complex;
function pclx(const magn, angle: Double): complex;

var
    cZERO, cONE: Complex;

implementation

function CMPLX(const a, b: Double): complex; inline;
begin
    Result.RE := A;
    Result.IM := B
end;

function CInv(const A: COMPLEX): COMPLEX; inline;
var
    DNOM: Double;
begin
    DNOM := A.RE * A.RE + A.IM * A.IM;
    Result.RE := A.RE / DNOM;
    Result.IM := (-A.IM) / DNOM
end;

function Cabs(const a: complex): Double; inline;
begin
    Result := SQRT(A.RE * A.RE + A.IM * A.IM)
end;

function Conjg(const a: complex): complex; inline;
begin
    Result.RE := A.RE;
    Result.im := -A.im;
end;

function ATAN2(x, iy: Double): Double;
const
    PI = 3.14159265359; { 180 DEGREES }
begin
    if (x < 0.0) and (iy >= 0) then
        Result := arctan(iy / x) + PI
    else
    if (x < 0.0) and (iy < 0) then
        Result := arctan(iy / x) - PI
    else
    if (x > 0.0) then
        Result := arctan(iy / x)
    else
    if (iy < 0.0) then
        Result := -PI / 2
    else
    if (iy > 0.0) then
        Result := PI / 2
    else
        Result := 0.0
end; { ATAN2 }

function CANG(const a: complex): Double;
begin
    Result := ATAN2(A.RE, A.IM)
end;

function CDANG(const a: complex): Double;
begin
    Result := ATAN2(A.RE, A.IM) * 57.29577951;
end;

function CtoPOLAR(const a: complex): polar;
begin
    with Result do
    begin
        MAG := Cabs(A);
        ANG := CANG(A)
    end;
end;

function CtoPOLARdeg(const a: complex): polar;
begin
    with Result do
    begin
        MAG := Cabs(A);
        ANG := CDANG(A)
    end;
end;

function CADD(const a, b: complex): complex; inline;
begin
    Result.RE := A.RE + B.RE;
    Result.IM := A.IM + B.IM
end;

procedure CACCUM(var a: complex; const b: complex); inline;
begin
    a.re := a.re + b.re;
    a.im := a.im + b.im;
end;

procedure CACCUMARRAY(a, b: pComplexArray; N: Smallint);
var
    i: Integer;
begin
    for i := 1 to N do
    begin
        a^[i].re := a^[i].re + b^[i].re;
        a^[i].im := a^[i].im + b^[i].im;
    end;
end;


function CSUB(const a, b: complex): complex; inline;
begin
    Result.RE := A.RE - B.RE;
    Result.IM := A.IM - B.IM
end;

function CMUL(const a, b: complex): complex; inline;
begin
    Result.RE := A.RE * B.RE - A.IM * B.IM;
    Result.IM := A.RE * B.IM + A.IM * B.RE
end;

function cmulreal(const a: complex; const b: Double): Complex;  { := a*b }
begin
    Result.re := a.re * b;
    Result.im := a.im * b;
end;

procedure cmulrealaccum(var a: complex; const b: Double); { a=a*b}
begin
    a.re := a.re * b;
    a.im := a.im * b;
end;

function CDIV(const a, b: complex): complex; inline;
var
    DNOM: Double;
begin
    DNOM := B.RE * B.RE + B.IM * B.IM;
    Result.RE := (A.RE * B.RE + A.IM * B.IM) / DNOM;
    Result.IM := (A.IM * B.RE - A.RE * B.IM) / DNOM
end;

function cdivreal(const a: complex; const b: Double): Complex; inline;  { := a /b}
begin
    Result.re := a.re / b;
    Result.im := a.im / b;
end;

function cnegate(const a: complex): complex; inline;

begin
    Result.re := -a.re;
    Result.im := -a.im;
end;

function csqrt(const a: complex): complex;
var
    x: Polar;
begin
      // algorithm: sqrt of magnitude/ half the angle
    x := ctopolar(A);
    Result := ptocomplex(topolar(sqrt(x.mag), x.ang / 2.0));
end;

function cln(const a: complex): complex;
var
    x: Polar;
begin
        // algorithm: ln of mag + j(angle), radians
    x := ctopolar(A);
    Result := cmplx(ln(x.mag), x.ang);
end;

function toPOLaR(const a, b: Double): polar; inline;
begin
    with Result do
    begin
        MAG := A;
        ANG := B;
    end;
end;

function PREL(const a: polar): Double;
begin
    Result := A.MAG * COS(A.ANG)
end;

function PIMG(const a: polar): Double;
begin
    Result := A.MAG * SIN(A.ANG)
end;

function PCLX(const magn, angle: Double): complex;
begin
    Result.RE := Magn * Cos(Angle);
    Result.IM := Magn * Sin(Angle);
end;

function PDEGtoCompLeX(const magn, angle: Double): complex;
var
    Ang: Double;
begin
    Ang := Angle / 57.29577951;
    with Result do
    begin
        RE := Magn * Cos(Ang);
        IM := Magn * Sin(Ang);
    end;
end;

function PtoCOMPLEX(const a: polar): complex;
begin
    with Result do
    begin
        RE := A.MAG * COS(A.ANG);
        IM := A.MAG * SIN(A.ANG);
    end;
end;

function PADD(const A, B: POLAR): POLAR;
begin
    Result := CtoPOLAR(CADD(PtoCOMPLEX(A), PtoCOMPLEX(B)))
end;

function PSUB(const a, b: polar): polar;
begin
    Result := CtoPOLAR(CSUB(PtoCOMPLEX(A), PtoCOMPLEX(B)))
end;

function PMUL(const a, b: polar): polar;
begin
    Result.MAG := A.MAG * B.MAG;
    Result.ANG := A.ANG + B.ANG
end;

function PDIV(const a, b: polar): polar;
begin
    Result.MAG := A.MAG / B.MAG;
    Result.ANG := A.ANG - B.ANG
end;


initialization

    cZERO := cmplx(0.0, 0.0);
    cONE := cmplx(1.0, 0.0);

end.
