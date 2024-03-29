unit DLineCodes;

interface

uses
  {$IFNDEF FPC_DLL}
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl,
{$ENDIF}
    LineCode;

function LineCodesI(mode: Longint; arg: Longint): Longint; CDECL;
function LineCodesF(mode: Longint; arg: Double): Double; CDECL;
function LineCodesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure LineCodesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
{$IFNDEF FPC_DLL}
    ComServ,
{$ENDIF}
    sysutils,
    DSSGlobals,
    LineUnits,
    ParserDel,
    Variants,
    Ucomplex;

{$IFDEF FPC_DLL}
const
    dssLineUnitsMaxnum = $00000009;  // from OpenDSSEngine_TLB.pas
{$ENDIF}

//*****************************Integer interface***************************************

function LineCodesI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pLineCode: TLineCodeObj;

begin
    Result := 0;
    case mode of
        0:
        begin  // LineCodes.Count
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := LineCodeClass.ElementCount;
        end;
        1:
        begin  // LineCodes.First
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := LineCodeClass.First;
        end;
        2:
        begin  // LineCodes.Next
            if ActiveCircuit[ActiveActor] <> NIL then
                Result := LineCodeClass.Next;
        end;
        3:
        begin  // LineCodes.Units Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.Units;
            end
        end;
        4:
        begin  // LineCodes.Units Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    if arg < dssLineUnitsMaxnum then
                    begin
                        Parser[ActiveActor].CmdString := Format('units=%s', [LineUnitsStr(arg)]);
                        Edit(ActiveActor);
                    end
                    else
                        DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.', 183);

                end;
            end;
        end;
        5:
        begin  // LineCodes.Phases Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.FNPhases;
            end
        end;
        6:
        begin  // LineCodes.Phases Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                pLineCode.NumPhases := arg;   // use property value to force reallocations
            end
        end;
        7:
        begin  // LineCodes.IsZ1Z0
            Result := 1;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                if pLineCode <> NIL then
                begin
                    if pLineCode.SymComponentsModel then
                        Result := 1
                    else
                        Result := 0;
                end;
            end;
        end
    else
    begin
        Result := -1;
    end;
    end;
end;

//*****************************Floating point interface***************************************

function LineCodesF(mode: Longint; arg: Double): Double; CDECL;
var
    pLineCode: TLineCodeObj;

begin
    Result := 0.0;
    case mode of
        0:
        begin  // LineCodes.R1 Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.R1;
            end
        end;
        1:
        begin  // LineCodes.R1 Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Parser[ActiveActor].CmdString := Format('R1=%g', [arg]);
                    Edit(ActiveActor);
                end;
            end;
        end;
        2:
        begin  // LineCodes.X1 Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.X1;
            end
        end;
        3:
        begin  // LineCodes.X1 Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Parser[ActiveActor].CmdString := Format('X1=%g', [arg]);
                    Edit(ActiveActor);
                end;
            end;
        end;
        4:
        begin  // LineCodes.R0 Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.R0;
            end
        end;
        5:
        begin  // LineCodes.R0 Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Parser[ActiveActor].CmdString := Format('R0=%g', [arg]);
                    Edit(ActiveActor);
                end;
            end;
        end;
        6:
        begin  // LineCodes.X0 Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.X0;
            end
        end;
        7:
        begin  // LineCodes.X0 Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Parser[ActiveActor].CmdString := Format('X0=%g', [arg]);
                    Edit(ActiveActor);
                end;
            end;
        end;
        8:
        begin  // LineCodes.C1 Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.C1 * 1.0e9;
            end
        end;
        9:
        begin  // LineCodes.C1 Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Parser[ActiveActor].CmdString := Format('C1=%g', [arg]);
                    Edit(ActiveActor);
                end;
            end;
        end;
        10:
        begin  // LineCodes.C0 Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.C0 * 1.0e9;
            end
        end;
        11:
        begin  // LineCodes.C0 Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Parser[ActiveActor].CmdString := Format('C0=%g', [arg]);
                    Edit(ActiveActor);
                end;
            end;
        end;
        12:
        begin  // LineCodes.NormAmps Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.NormAmps;
            end
        end;
        13:
        begin  // LineCodes.NormAmps Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                pLineCode.NormAmps := arg;
            end
        end;
        14:
        begin  // LineCodes.EmergAmps Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                Result := pLineCode.EmergAmps;
            end
        end;
        15:
        begin  // LineCodes.NormAmps Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                pLineCode.EmergAmps := arg;
            end
        end
    else
    begin
        Result := -1.0;
    end;
    end;
end;

//*****************************String interface***************************************

function LineCodesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
var
    pLineCode: TLineCodeObj;

begin
    Result := '';
    case mode of
        0:
        begin  // LineCodes.Name Read
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                if pLineCode <> NIL then
                begin
                    Result := Pansichar(Ansistring(pLineCode.Name));
                end;
            end;
        end;
        1:
        begin  // LineCodes.Name Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if not LineCodeClass.SetActive(String(arg)) then
                    DoSimpleMsg('LineCode "' + arg + '" Not Found in Active Circuit.', 51008);

               // Still same active object if not found
            end;
        end
    else
    begin
        Result := Pansichar(Ansistring('Parameter not identified'));
    end;
    end;
end;

//*****************************Variants interface***************************************

procedure LineCodesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;
var
    pLineCode: TLineCodeObj;
    i,
    j,
    k: Integer;
    Ztemp: Complex;
    Factor: Double;
    PDouble: ^Double;

begin
    case mode of
        0:
        begin  // LineCodes.Rmatrix Read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    setlength(myDBLArray, Sqr(FNphases));
                    k := 0;
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to FNphases do
                        begin
                            myDBLArray[k] := Z.GetElement(i, j).re;
                            Inc(k);
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        1:
        begin  // LineCodes.Rmatrix Write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to FNphases do
                        begin
                            PDouble := myPointer;
                            ZTemp := Z.GetElement(i, j);
                            Z.SetElement(i, j, Cmplx(PDouble^, ZTemp.im));
                            Inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;
                    end;
                end;
            end;
            mySize := k;
        end;
        2:
        begin  // LineCodes.Xmatrix Read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    setlength(myDBLArray, Sqr(FNphases));
                    k := 0;
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to FNphases do
                        begin
                            myDBLArray[k] := Z.GetElement(i, j).im;
                            Inc(k);
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        3:
        begin  // LineCodes.Xmatrix Write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to FNphases do
                        begin
                            PDouble := myPointer;
                            ZTemp := Z.GetElement(i, j);
                            Z.SetElement(i, j, Cmplx(ZTemp.re, PDouble^));
                            Inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;
                    end;
                end;
            end;
            mySize := k;
        end;
        4:
        begin  // LineCodes.Cmatrix Read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Factor := (TwoPi * BaseFrequency * 1.0e-9);
                    setlength(myDBLArray, Sqr(FNphases));
                    k := 0;
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to FNphases do
                        begin
                            myDBLArray[k] := YC.GetElement(i, j).im / Factor;
                            Inc(k);
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        5:
        begin  // LineCodes.Cmatrix Write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Factor := TwoPi * BaseFrequency * 1.0e-9;
                    for i := 1 to FNPhases do
                    begin
                        for j := 1 to FNphases do
                        begin
                            PDouble := myPointer;
                            Yc.SetElement(i, j, Cmplx(0.0, (PDouble^) * Factor));
                            Inc(k);
                            inc(Pbyte(myPointer), 8);
                        end;
                    end;
                end;
            end;
            mySize := k;
        end;
        6:
        begin  // LineCodes.AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            mySize := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if LineCodeClass.ElementList.ListSize > 0 then
                    begin
                        pLineCode := LineCodeClass.ElementList.First;
                        while pLineCode <> NIL do
                        begin
                            WriteStr2Array(pLineCode.Name);
                            WriteStr2Array(Char(0));
                            ;
                            pLineCode := LineCodeClass.ElementList.Next;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
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
