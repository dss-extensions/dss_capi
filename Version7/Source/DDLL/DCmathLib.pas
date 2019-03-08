unit DCmathLib;

interface

function CmathLibF(mode: Longint; arg1: Double; arg2: Double): Double; CDECL;
procedure CmathLibV(mode: Longint; Realpart: Double; ImagPart: Double; out arg: Variant); CDECL;

implementation

uses
    Ucomplex,
    variants;

function CmathLibF(mode: Longint; arg1: Double; arg2: Double): Double; CDECL;
begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // CmathLib.Cabs
            Result := cabs(cmplx(arg1, arg2));
        end;
        1:
        begin
            Result := cdang(cmplx(arg1, arg2));
        end
    else
        Result := -1.0;
    end;
end;

//***************************Variant type properties****************************
procedure CmathLibV(mode: Longint; Realpart: Double; ImagPart: Double; out arg: Variant); CDECL;

var
    TempPolar: polar;
    cTemp: Complex;

begin
    case mode of
        0:
        begin  // CmathLib.Cmplx
            arg := VarArrayCreate([0, 1], varDouble);
            arg[0] := RealPart;
            arg[1] := ImagPart;
        end;
        1:
        begin  // CmathLib.ctopolardeg
            arg := VarArrayCreate([0, 1], varDouble);
            TempPolar := ctopolardeg(cmplx(RealPart, ImagPart));
            arg[0] := TempPolar.mag;
            arg[1] := TempPolar.ang;
        end;
        2:
        begin  // CmathLib.pdegtocomplex
            arg := VarArrayCreate([0, 1], varDouble);
            cTemp := pdegtocomplex(RealPart, ImagPart);
            arg[0] := cTemp.re;
            arg[1] := cTemp.im;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
