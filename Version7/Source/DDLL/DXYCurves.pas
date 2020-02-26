unit DXYCurves;

interface

uses
    XYCurve,
    DSSClass,
    Arraydef,
    UComplex,
    Solution;

function XYCurvesI(mode: Longint; arg: Longint): Longint; CDECL;
function XYCurvesF(mode: Longint; arg: Double): Double; CDECL;
function XYCurvesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure XYCurvesV(mode: Longint; var arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    DSSObject,
    Variants;

function XYCurvesI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pXYCurve: TXYCurveObj;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // XYCurves.Count
            Result := 0;
            if ActiveCircuit <> NIL then
                Result := XYCurveClass.ElementCount;
        end;
        1:
        begin  // XYCurves.First
            Result := 0;
            if ActiveCircuit <> NIL then
                Result := XYCurveClass.First;
        end;
        2:
        begin  // XYCurves.Next
            Result := 0;
            if ActiveCircuit <> NIL then
                Result := XYCurveClass.Next;
        end;
        3:
        begin  // XYCurves.Npts read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pXYCurve.NumPoints;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51009);
                end;
            end;
        end;
        4:
        begin  // XYCurves.Npts write
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    pXYCurve.NumPoints := arg;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51014);
                end;
            end;
        end
    else
        Result := -1;
    end;
end;

//************************Floating point type properties******************************
function XYCurvesF(mode: Longint; arg: Double): Double; CDECL;

var
    pXYCurve: TXYCurveObj;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // XYCurve.X read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pXYCurve.X;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51010);
                end;
            end;
        end;
        1:
        begin  // XYCurve.X write
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    pXYCurve.X := arg;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51010);
                end;
            end;
        end;
        2:
        begin  // XYCurve.Y read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pXYCurve.FYscale;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51011);
                end;
            end;
        end;
        3:
        begin  // XYCurve.Y write
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    pXYCurve.FYScale := arg;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51010);
                end;
            end;
        end;
        4:
        begin  // XYCurve.XShift read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pXYCurve.FXshift;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51011);
                end;
            end;
        end;
        5:
        begin  // XYCurve.XShift write
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    pXYCurve.FXShift := arg;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51010);
                end;
            end;
        end;
        6:
        begin  // XYCurve.YShift read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pXYCurve.FYshift;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51011);
                end;
            end;
        end;
        7:
        begin  // XYCurve.YShift write
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    pXYCurve.FYShift := arg;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51010);
                end;
            end;
        end;
        8:
        begin  // XYCurve.XScale read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pXYCurve.FXscale;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51011);
                end;
            end;
        end;
        9:
        begin  // XYCurve.XScale write
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    pXYCurve.FXScale := arg;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51010);
                end;
            end;
        end;
        10:
        begin  // XYCurve.YScale read
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pXYCurve.FYscale;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51011);
                end;
            end;
        end;
        11:
        begin  // XYCurve.YScale write
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    pXYCurve.FYScale := arg;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51010);
                end;
            end;
        end
    else
        Result := -1.0;
    end;
end;

//************************String type properties***********************************
function XYCurvesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    pXYCurve: TXYCurveObj;

begin
    Result := pAnsiChar(Ansistring(''));  // Default return value
    case mode of
        0:
        begin  // XYCurve.Name read
            Result := pAnsiChar(Ansistring(''));  // means no name
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := pAnsiChar(Ansistring(pXYCurve.Name));
                end;
            end;
        end;
        1:
        begin  // XYCurve.Name write
            if ActiveCircuit <> NIL then
            begin
                if not XYCurveClass.SetActive(String(arg)) then
                    DoSimpleMsg('XYCurve "' + String(arg) + '" Not Found in Active Circuit.', 51008);
         // Still same active object if not found
            end;
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

//************************Variant type properties********************************
procedure XYCurvesV(mode: Longint; var arg: Variant); CDECL;

var
    pXYCurve: TXYCurveObj;
    k: Integer;
    i, LoopLimit: Integer;

begin
    case mode of
        0:
        begin  // XYCurve.XArray read
            arg := VarArrayCreate([0, 0], varDouble);
            arg[0] := 0.0;  // error condition: one element array=0
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    VarArrayRedim(arg, pXYCurve.NumPoints - 1);
                    for k := 0 to pXYCurve.NumPoints - 1 do
                        arg[k] := pXYCurve.XValue_pt[k + 1];
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51013);
                end;
            end;
        end;
        1:
        begin  // XYCurve.XArray write
 //     arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin

          // Only put in as many points as we have allocated
                    LoopLimit := VarArrayHighBound(arg, 1);
                    if (LoopLimit - VarArrayLowBound(arg, 1) + 1) > pXYCurve.NumPoints then
                        LoopLimit := VarArrayLowBound(arg, 1) + pXYCurve.NumPoints - 1;
//             DoSimpleMsg('We are in',0);
                    k := 1;
                    for i := VarArrayLowBound(arg, 1) to LoopLimit do
                    begin
                        pXYCurve.XValue_pt[k] := arg[i];
                        inc(k);
                    end;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51015);
                end;
            end;
        end;
        2:
        begin  // XYCurve.YArray read
            arg := VarArrayCreate([0, 0], varDouble);
            arg[0] := 0.0;  // error condition: one element array=0
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    VarArrayRedim(arg, pXYCurve.NumPoints - 1);
                    for k := 0 to pXYCurve.NumPoints - 1 do
                        arg[k] := pXYCurve.YValue_pt[k + 1];
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51013);
                end;
            end;
        end;
        3:
        begin  // XYCurve.YArray write
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit <> NIL then
            begin
                pXYCurve := XYCurveClass.GetActiveObj;
                if pXYCurve <> NIL then
                begin

        // Only put in as many points as we have allocated
                    LoopLimit := VarArrayHighBound(arg, 1);
                    if (LoopLimit - VarArrayLowBound(arg, 1) + 1) > pXYCurve.NumPoints then
                        LoopLimit := VarArrayLowBound(arg, 1) + pXYCurve.NumPoints - 1;

                    k := 1;
                    for i := VarArrayLowBound(arg, 1) to LoopLimit do
                    begin
                        pXYCurve.YValue_pt[k] := arg[i];
                        inc(k);
                    end;

                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51016);
                end;
            end;
        end
    else
        arg[0] := 'Error, parameter not valid';
    end;
end;

end.
