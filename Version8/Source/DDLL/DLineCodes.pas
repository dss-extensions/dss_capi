unit DLineCodes;

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl,
    LineCode;

function LineCodesI(mode: Longint; arg: Longint): Longint; CDECL;
function LineCodesF(mode: Longint; arg: Double): Double; CDECL;
function LineCodesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure LineCodesV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    ComServ,
    sysutils,
    DSSGlobals,
    LineUnits,
    ParserDel,
    Variants,
    Ucomplex;

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
                Result := pLineCode.C1;
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
                Result := pLineCode.C0;
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

function LineCodesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
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
                    Result := pAnsiChar(Ansistring(pLineCode.Name));
                end;
            end;
        end;
        1:
        begin  // LineCodes.Name Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if not LineCodeClass.SetActive(arg) then
                    DoSimpleMsg('LineCode "' + arg + '" Not Found in Active Circuit.', 51008);

               // Still same active object if not found
            end;
        end
    else
    begin
        Result := pAnsiChar(Ansistring('Parameter not identified'));
    end;
    end;
end;

//*****************************Variants interface***************************************

procedure LineCodesV(mode: Longint; out arg: Variant); CDECL;
var
    pLineCode: TLineCodeObj;
    i, j, k: Integer;
    Ztemp: complex;
    Factor: Double;

begin
    case mode of
        0:
        begin  // LineCodes.Rmatrix Read
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    arg := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
                    k := 0;
                    for i := 1 to FNPhases do
                        for j := 1 to FNphases do
                        begin
                            arg[k] := Z.GetElement(i, j).re;
                            Inc(k);
                        end;
                end;
            end;
        end;
        1:
        begin  // LineCodes.Rmatrix Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    k := VarArrayLowBound(arg, 1);
                    for i := 1 to FNPhases do
                        for j := 1 to FNphases do
                        begin
                            ZTemp := Z.GetElement(i, j);
                            Z.SetElement(i, j, Cmplx(arg[k], ZTemp.im));
                            Inc(k);
                        end;
                end;
            end;
        end;
        2:
        begin  // LineCodes.Xmatrix Read
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    arg := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
                    k := 0;
                    for i := 1 to FNPhases do
                        for j := 1 to FNphases do
                        begin
                            arg[k] := Z.GetElement(i, j).im;
                            Inc(k);
                        end;
                end;
            end;
        end;
        3:
        begin  // LineCodes.Xmatrix Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    k := VarArrayLowBound(arg, 1);
                    for i := 1 to FNPhases do
                        for j := 1 to FNphases do
                        begin
                            ZTemp := Z.GetElement(i, j);
                            Z.SetElement(i, j, Cmplx(ZTemp.re, arg[k]));
                            Inc(k);
                        end;
                end;
            end;
        end;
        4:
        begin  // LineCodes.Cmatrix Read
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Factor := (TwoPi * BaseFrequency * 1.0e-9);
                    arg := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
                    k := 0;
                    for i := 1 to FNPhases do
                        for j := 1 to FNphases do
                        begin
                            arg[k] := YC.GetElement(i, j).im / Factor;
                            Inc(k);
                        end;
                end;
            end;
        end;
        5:
        begin  // LineCodes.Cmatrix Write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLineCode := LineCodeClass.GetActiveObj;
                with pLineCode do
                begin
                    Factor := TwoPi * BaseFrequency * 1.0e-9;
                    k := VarArrayLowBound(arg, 1);
                    for i := 1 to FNPhases do
                        for j := 1 to FNphases do
                        begin
                            Yc.SetElement(i, j, Cmplx(0.0, arg[k] * Factor));
                            Inc(k);
                        end;
                end;
            end;
        end;
        6:
        begin  // LineCodes.AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit[ActiveActor] <> NIL then
                with ActiveCircuit[ActiveActor] do
                    if LineCodeClass.ElementList.ListSize > 0 then
                    begin
                        VarArrayRedim(arg, LineCodeClass.ElementList.ListSize - 1);
                        k := 0;
                        pLineCode := LineCodeClass.ElementList.First;
                        while pLineCode <> NIL do
                        begin
                            arg[k] := pLineCode.Name;
                            Inc(k);
                            pLineCode := LineCodeClass.ElementList.Next;
                        end;
                    end;
        end
    else
    begin
        arg := VarArrayCreate([0, 0], varOleStr);
        arg[0] := 'Parameter not identified';
    end;

    end;
end;

end.
