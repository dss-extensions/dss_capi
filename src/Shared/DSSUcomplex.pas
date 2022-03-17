unit DSSUcomplex;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses UComplex;

type
    Complex = UComplex.Complex;
    PComplex = UComplex.PComplex;
    pComplexArray = ^ComplexArray;
    ComplexArray = array [1..100] of Complex;

    polar = record
        mag, ang: Double;
    end;

const
    CDOUBLEONE: Complex = (re: 1.0; im: 1.0);
    cZERO: Complex = (re: 0.0; im: 0.0);
    cONE: Complex = (re: 1.0; im: 0.0);

function cmplx(const a, b: Double): complex; inline;
function cabs(const a: complex): Double; inline;
Function cabs2(const a:complex):double; // best when you don't need sqrt -- TODO: rename?
function cang(const a: complex): Double;
function cdang(const a: complex): Double; // angle of complex number, degrees
function ctopolar(const a: complex): polar;
function ctopolardeg(const a: complex): polar;  // complex to polar, degrees
function topolar(const a, b: Double): polar;  // scalar to polar
function ptocomplex(const a: polar): complex;
function pdegtocomplex(const magn, angle: Double): complex;
function pclx(const magn, angle: Double): complex;

implementation

function CMPLX(const a, b: Double): complex; inline;
begin
    Result.RE := A;
    Result.IM := B
end;

function Cabs(const a: complex): Double; inline;
begin
    Result := SQRT(A.RE * A.RE + A.IM * A.IM)
end;

function Cabs2(const a: complex): Double;
begin
    Result := (A.RE * A.RE + A.IM * A.IM)
end;

function ATAN2(x, iy: Double): Double;
const
    PI = 3.14159265359; { 180 DEGREES } // TODO: remove for 0.13
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
end;

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

function toPOLaR(const a, b: Double): polar;
begin
    with Result do
    begin
        MAG := A;
        ANG := B;
    end;
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

end.
