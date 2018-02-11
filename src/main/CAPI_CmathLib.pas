UNIT CAPI_CmathLib;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE CmathLib_Get_cmplx(var ResultPtr: PDouble; var ResultCount: Integer; RealPart, ImagPart: Double);cdecl;
function CmathLib_Get_cabs(realpart, imagpart: Double):Double;cdecl;
function CmathLib_Get_cdang(RealPart, ImagPart: Double):Double;cdecl;
PROCEDURE CmathLib_Get_ctopolardeg(var ResultPtr: PDouble; var ResultCount: Integer; RealPart, ImagPart: Double);cdecl;
PROCEDURE CmathLib_Get_pdegtocomplex(var ResultPtr: PDouble; var ResultCount: Integer; magnitude, angle: Double);cdecl;
PROCEDURE CmathLib_Get_cmul(var ResultPtr: PDouble; var ResultCount: Integer; a1, b1, a2, b2: Double);cdecl;
PROCEDURE CmathLib_Get_cdiv(var ResultPtr: PDouble; var ResultCount: Integer; a1, b1, a2, b2: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Ucomplex;

PROCEDURE CmathLib_Get_cmplx(var ResultPtr: PDouble; var ResultCount: Integer; RealPart, ImagPart: Double);cdecl;
VAR
    Result: PDoubleArray;

begin
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      Result[0] := RealPart;
      Result[1] := ImagPart;
end;
//------------------------------------------------------------------------------
function CmathLib_Get_cabs(realpart, imagpart: Double):Double;cdecl;
begin
     Result := cabs(cmplx(realpart, imagpart));
end;
//------------------------------------------------------------------------------
function CmathLib_Get_cdang(RealPart, ImagPart: Double):Double;cdecl;
begin
     Result := cdang(cmplx(realpart, imagpart));
end;
//------------------------------------------------------------------------------
PROCEDURE CmathLib_Get_ctopolardeg(var ResultPtr: PDouble; var ResultCount: Integer; RealPart, ImagPart: Double);cdecl;
VAR
  Result: PDoubleArray;
   TempPolar:polar;
begin
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      TempPolar := ctopolardeg(cmplx(RealPart, ImagPart));
      Result[0] := TempPolar.mag;
      Result[1] := TempPolar.ang;
end;
//------------------------------------------------------------------------------
PROCEDURE CmathLib_Get_pdegtocomplex(var ResultPtr: PDouble; var ResultCount: Integer; magnitude, angle: Double);cdecl;
VAR
  Result: PDoubleArray;
   cTemp : Complex;
begin
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      cTemp := pdegtocomplex(magnitude, angle);
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;
//------------------------------------------------------------------------------
PROCEDURE CmathLib_Get_cmul(var ResultPtr: PDouble; var ResultCount: Integer; a1, b1, a2, b2: Double);cdecl;
VAR
  Result: PDoubleArray;
   cTemp : Complex;
begin
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      cTemp := cmul(cmplx(a1, b1), cmplx(a2, b2));
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;
//------------------------------------------------------------------------------
PROCEDURE CmathLib_Get_cdiv(var ResultPtr: PDouble; var ResultCount: Integer; a1, b1, a2, b2: Double);cdecl;
VAR
  Result: PDoubleArray;
   cTemp : Complex;
begin
      Result := DSS_CreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      cTemp := cdiv(cmplx(a1, b1), cmplx(a2, b2));
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;
//------------------------------------------------------------------------------
END.
