unit DXYCurves;

interface

uses
  {$IFNDEF FPC_DLL}
    ActiveX,
{$ENDIF}
    XYCurve,
    DSSClass,
    Arraydef,
    UComplex,
    Solution;

function XYCurvesI(mode: Longint; arg: Longint): Longint; CDECL;
function XYCurvesF(mode: Longint; arg: Double): Double; CDECL;
function XYCurvesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure XYCurvesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
{$ENDIF}
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
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := XYCurveClass[ActiveActor].ElementCount;
        end;
        1:
        begin  // XYCurves.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := XYCurveClass[ActiveActor].First;
        end;
        2:
        begin  // XYCurves.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := XYCurveClass[ActiveActor].Next;
        end;
        3:
        begin  // XYCurves.Npts read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
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
function XYCurvesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    pXYCurve: TXYCurveObj;

begin
    Result := Pansichar(Ansistring(''));  // Default return value
    case mode of
        0:
        begin  // XYCurve.Name read
            Result := Pansichar(Ansistring(''));  // means no name
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    Result := Pansichar(Ansistring(pXYCurve.Name));
                end;
            end;
        end;
        1:
        begin  // XYCurve.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if not XYCurveClass[ActiveActor].SetActive(String(arg)) then
                    DoSimpleMsg('XYCurve "' + arg + '" Not Found in Active Circuit.', 51008);
         // Still same active object if not found
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//************************Variant type properties********************************
procedure XYCurvesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    pXYCurve: TXYCurveObj;
    k,
    i,
    LoopLimit: Integer;
    PDouble: ^Double;

begin
    case mode of
        0:
        begin  // XYCurve.XArray read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    setlength(myDBLArray, pXYCurve.NumPoints);
                    for k := 0 to pXYCurve.NumPoints - 1 do
                        myDBLArray[k] := pXYCurve.XValue_pt[k + 1];
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51013);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        1:
        begin  // XYCurve.XArray write
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
                if pXYCurve <> NIL then
                begin
        // Only put in as many points as we have allocated
                    LoopLimit := pXYCurve.NumPoints;
                    for i := 1 to LoopLimit do
                    begin
                        PDouble := myPointer;
                        pXYCurve.XValue_pt[k] := PDouble^;
                        inc(Pbyte(myPointer), 8);
                        inc(k);
                    end;
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51015);
                end;
            end;
            mySize := k - 1;
        end;
        2:
        begin  // XYCurve.YArray read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
                if pXYCurve <> NIL then
                begin
                    setlength(myDBLArray, pXYCurve.NumPoints);
                    for k := 0 to (pXYCurve.NumPoints - 1) do
                        myDBLArray[k] := pXYCurve.YValue_pt[k + 1];
                end
                else
                begin
                    DoSimpleMsg('No active XYCurve Object found.', 51013);
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        3:
        begin  // XYCurve.YArray write
            myType := 2;        // Double
            k := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pXYCurve := XYCurveClass[ActiveActor].GetActiveObj;
                if pXYCurve <> NIL then
                begin

          // Only put in as many points as we have allocated
                    LoopLimit := pXYCurve.NumPoints;
                    for i := 1 to LoopLimit do
                    begin
                        PDouble := myPointer;
                        pXYCurve.YValue_pt[k] := PDouble^;
                        inc(Pbyte(myPointer), 8);
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
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

end.
