UNIT CAPI_CmathLib;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE CmathLib_Get_cmplx(var ResultPtr: PDouble; ResultCount: PInteger; RealPart, ImagPart: Double);cdecl;
PROCEDURE CmathLib_Get_cmplx_GR(RealPart, ImagPart: Double);cdecl;
function CmathLib_Get_cabs(realpart, imagpart: Double):Double;cdecl;
function CmathLib_Get_cdang(RealPart, ImagPart: Double):Double;cdecl;
PROCEDURE CmathLib_Get_ctopolardeg(var ResultPtr: PDouble; ResultCount: PInteger; RealPart, ImagPart: Double);cdecl;
PROCEDURE CmathLib_Get_ctopolardeg_GR(RealPart, ImagPart: Double);cdecl;
PROCEDURE CmathLib_Get_pdegtocomplex(var ResultPtr: PDouble; ResultCount: PInteger; magnitude, angle: Double);cdecl;
PROCEDURE CmathLib_Get_pdegtocomplex_GR(magnitude, angle: Double);cdecl;
PROCEDURE CmathLib_Get_cmul(var ResultPtr: PDouble; ResultCount: PInteger; a1, b1, a2, b2: Double);cdecl;
PROCEDURE CmathLib_Get_cmul_GR(a1, b1, a2, b2: Double);cdecl;
PROCEDURE CmathLib_Get_cdiv(var ResultPtr: PDouble; ResultCount: PInteger; a1, b1, a2, b2: Double);cdecl;
PROCEDURE CmathLib_Get_cdiv_GR(a1, b1, a2, b2: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, Ucomplex;

PROCEDURE CmathLib_Get_cmplx(var ResultPtr: PDouble; ResultCount: PInteger; RealPart, ImagPart: Double);cdecl;
VAR
    Result: PDoubleArray;

begin
      Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      Result[0] := RealPart;
      Result[1] := ImagPart;
end;
PROCEDURE CmathLib_Get_cmplx_GR(RealPart, ImagPart: Double);cdecl;
// Same as CmathLib_Get_cmplx but uses global result (GR) pointers
begin
   CmathLib_Get_cmplx(GR_DataPtr_PDouble, GR_CountPtr_PDouble, RealPart, ImagPart)
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
PROCEDURE CmathLib_Get_ctopolardeg(var ResultPtr: PDouble; ResultCount: PInteger; RealPart, ImagPart: Double);cdecl;
VAR
  Result: PDoubleArray;
   TempPolar:polar;
begin
      Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      TempPolar := ctopolardeg(cmplx(RealPart, ImagPart));
      Result[0] := TempPolar.mag;
      Result[1] := TempPolar.ang;
end;
PROCEDURE CmathLib_Get_ctopolardeg_GR(RealPart, ImagPart: Double);cdecl;
// Same as CmathLib_Get_ctopolardeg but uses global result (GR) pointers
begin
   CmathLib_Get_ctopolardeg(GR_DataPtr_PDouble, GR_CountPtr_PDouble, RealPart, ImagPart)
end;

//------------------------------------------------------------------------------
PROCEDURE CmathLib_Get_pdegtocomplex(var ResultPtr: PDouble; ResultCount: PInteger; magnitude, angle: Double);cdecl;
VAR
  Result: PDoubleArray;
   cTemp : Complex;
begin
      Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      cTemp := pdegtocomplex(magnitude, angle);
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;
PROCEDURE CmathLib_Get_pdegtocomplex_GR(magnitude, angle: Double);cdecl;
// Same as CmathLib_Get_pdegtocomplex but uses global result (GR) pointers
begin
   CmathLib_Get_pdegtocomplex(GR_DataPtr_PDouble, GR_CountPtr_PDouble, magnitude, angle)
end;

//------------------------------------------------------------------------------
PROCEDURE CmathLib_Get_cmul(var ResultPtr: PDouble; ResultCount: PInteger; a1, b1, a2, b2: Double);cdecl;
VAR
  Result: PDoubleArray;
   cTemp : Complex;
begin
      Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      cTemp := cmul(cmplx(a1, b1), cmplx(a2, b2));
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;
PROCEDURE CmathLib_Get_cmul_GR(a1, b1, a2, b2: Double);cdecl;
// Same as CmathLib_Get_cmul but uses global result (GR) pointers
begin
   CmathLib_Get_cmul(GR_DataPtr_PDouble, GR_CountPtr_PDouble, a1, b1, a2, b2)
end;

//------------------------------------------------------------------------------
PROCEDURE CmathLib_Get_cdiv(var ResultPtr: PDouble; ResultCount: PInteger; a1, b1, a2, b2: Double);cdecl;
VAR
  Result: PDoubleArray;
   cTemp : Complex;
begin
      Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (1) + 1);
      cTemp := cdiv(cmplx(a1, b1), cmplx(a2, b2));
      Result[0] := cTemp.re;
      Result[1] := cTemp.im;
end;
PROCEDURE CmathLib_Get_cdiv_GR(a1, b1, a2, b2: Double);cdecl;
// Same as CmathLib_Get_cdiv but uses global result (GR) pointers
begin
   CmathLib_Get_cdiv(GR_DataPtr_PDouble, GR_CountPtr_PDouble, a1, b1, a2, b2)
end;

//------------------------------------------------------------------------------
END.
