unit DLines;

interface

function LinesI(mode: Longint; arg: Longint): Longint; CDECL;
function LinesF(mode: Longint; arg: Double): Double; CDECL;
function LinesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure LinesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    Line,
    DSSClassDefs,
    DSSGlobals,
    CktElement,
    XYCurve,
    uComplex,
    ExecHelper,
{$IFNDEF FPC_DLL}
    dialogs,
{$ENDIF}
    Sysutils,
    ParserDel,
    Variants,
    Math,
    LineUnits;

function IsLine(const CktElem: TDSSCktElement): Boolean;

begin
    Result := ((CktElem.DssObjtype and CLASSMASK) = LINE_ELEMENT);
    if not Result then
        DoSimpleMsg('Line Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
            'Element name=' + CktElem.Name, 5007);
end;

function LinesI(mode: Longint; arg: Longint): Longint; CDECL;

var
    pLine: TLineObj;

begin
    Result := 0;
    case mode of
        0:
        begin  // Lines.First
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLine := ActiveCircuit[ActiveActor].Lines.First;
                if pLine <> NIL then
                begin
                    repeat
                        if pLine.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
                            Result := 1;
                        end
                        else
                            pLine := ActiveCircuit[ActiveActor].Lines.Next;
                    until (Result = 1) or (pLine = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin  // Lines.Next
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLine := ActiveCircuit[ActiveActor].Lines.Next;
                if pLine <> NIL then
                begin
                    repeat
                        if pLine.Enabled then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
                            Result := ActiveCircuit[ActiveActor].Lines.ActiveIndex;
                        end
                        else
                            pLine := ActiveCircuit[ActiveActor].Lines.Next;
                    until (Result > 0) or (pLine = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // Lines.Phases read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := ActiveCircuit[ActiveActor].ActiveCktElement.Nphases;
                end
        end;
        3:
        begin  // Lines.Phases write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Nphases := arg;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        4:
        begin  // Lines.NumCust
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).BranchNumCustomers;
                end
        end;
        5:
        begin  // Lines.Parent
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    pLine := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement);
                    if pLine.ParentPDelement <> NIL then
                    begin
                        if (pLine.ParentPDelement.Enabled) and (IsLine(pLine.ParentPDelement)) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pLine.ParentPDElement;
                            Result := ActiveCircuit[ActiveActor].Lines.ActiveIndex;
                        end;
                    end;
                end;
        end;
        6:
        begin  // Lines.Count
            if Assigned(ActiveCircuit[ActiveActor]) then
                Result := ActiveCircuit[ActiveActor].Lines.ListSize;
        end;
        7:
        begin  // Lines.Units read
            Result := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).LengthUnits;
                end
        end;
        8:
        begin  // Line.Units write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        if arg < 9 then
                        begin
                            Parser[ActiveActor].CmdString := Format('units=%s', [LineUnitsStr(arg)]);
                            Edit(ActiveActor);
                            YprimInvalid[ActiveActor] := TRUE;
                        end
                        else
                            DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.', 183);
                    end;
                end;
        end
    else
        Result := -1;
    end;
end;

//******************************floating point type properties*************************
function LinesF(mode: Longint; arg: Double): Double; CDECL;
var
    RatingIdx: Integer;
    RSignal: TXYCurveObj;
begin
    Result := 0.0;
    case mode of
        0:
        begin  // Lines.Length read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).Len;
                end
        end;
        1:
        begin  // Lines.Length write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Len := arg;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        2:
        begin  // Lines.R1 read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Result := R1 / UnitsConvert;
                    end;
        end;
        3:
        begin  // Lines.R1 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        R1 := arg * UnitsConvert;
                        SymComponentsChanged := TRUE;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        4:
        begin  // Lines.X1 read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Result := X1 / UnitsConvert;
                    end;
        end;
        5:
        begin  // Lines.X1 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        X1 := arg * UnitsConvert;
                        SymComponentsChanged := TRUE;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        6:
        begin  // Lines.R0 read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Result := R0 / UnitsConvert;
                    end;
        end;
        7:
        begin  // Lines.R0 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        R0 := arg * UnitsConvert;
                        SymComponentsChanged := TRUE;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        8:
        begin  // Lines.X0 read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Result := X0 / UnitsConvert;
                    end;
        end;
        9:
        begin  // Lines.X0 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        X0 := arg * UnitsConvert;
                        SymComponentsChanged := TRUE;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        10:
        begin  // Lines.C1 read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Result := C1 / UnitsConvert * 1.0e9;
                    end;
        end;
        11:
        begin  // Lines.C1 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        C1 := arg * 1.0e-9 * UnitsConvert;
                        SymComponentsChanged := TRUE;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        12:
        begin // Lines.C0 read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Result := C0 / UnitsConvert * 1.0e9;
                    end
        end;
        13:
        begin  // Line.C0 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        C0 := arg * 1.0e-9 * UnitsConvert;
                        SymComponentsChanged := TRUE;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        14:
        begin  // Lines.NormAmps read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).NormAmps;
                end
        end;
        15:
        begin  // Lines.NormAmps write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        NormAmps := arg;
                    end;
                end;
        end;
        16:
        begin  // Lines.EmergAmps read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).EmergAmps;
                end
        end;
        17:
        begin  // Lines.EmergAmps write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        EmergAmps := arg;
                    end;
                end;
        end;
        18:
        begin  // Lines.Rg read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).Rg;
                end
        end;
        19:
        begin  // Lines.Rg write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Parser[ActiveActor].CmdString := Format('rg=%.7g', [arg]);
                        Edit(ActiveActor);
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        20:
        begin  // Lines.Xg read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).Xg;
                end
        end;
        21:
        begin  // Lines.Xg write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Parser[ActiveActor].CmdString := Format('xg=%.7g', [arg]);
                        Edit(ActiveActor);
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        22:
        begin  // Lines.Rho read
            Result := 0.0;
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).rho;
                end
        end;
        23:
        begin  // Lines.Rho write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Parser[ActiveActor].CmdString := Format('rho=%.7g', [arg]);
                        Edit(ActiveActor);
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        24:
        begin  // Lines.SeasonRating
            if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
            begin
                if SeasonalRating then
                begin
                    if (SeasonSignal <> '') then
                    begin
                        RSignal := XYCurveClass[ActiveActor].Find(SeasonSignal);
                        if RSignal <> NIL then
                            RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit[ActiveActor].Solution.DynaVars.intHour));
            // Just in case
                        if RatingIdx > (TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).NumAmpRatings - 1) then
                            Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).NormAmps
                        else
                            Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).AmpRatings[RatingIdx];
                    end
                    else
                        Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).NormAmps;
                end
                else
                    Result := TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).NormAmps;
            end
            else
                Result := 0.0;
        end
    else
        Result := -1.0;
    end;
end;

//******************************String type properties****************************
function LinesS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    pLine: TDSSCktElement;
    activesave: Integer;
    S: String;
    Found: Boolean;

begin
    Result := Pansichar(Ansistring(''));
    case mode of
        0:
        begin  // Lines.Name read
            Result := Pansichar(Ansistring(''));  // signify no name
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                pLine := ActiveCircuit[ActiveActor].ActiveCktElement;
                if pLine <> NIL then
                begin
                    Result := Pansichar(Ansistring(pLine.Name));
                end;
            end;
        end;
        1:
        begin  // Lines.Name write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin      // Search list of Lines in active circuit for name
                with ActiveCircuit[ActiveActor].Lines do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pLine := First;
                    while pLine <> NIL do
                    begin
                        if (CompareText(pLine.Name, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
                            Found := TRUE;
                            Break;
                        end;
                        pLine := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Line "' + S + '" Not Found in Active Circuit.', 5008);
                        pLine := Get(ActiveSave);    // Restore active Line
                        ActiveCircuit[ActiveActor].ActiveCktElement := pLine;
                    end;
                end;
            end;
        end;
        2:
        begin  // Lines.Bus1 read
            Result := Pansichar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := Pansichar(Ansistring(ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(1)));
                end
        end;
        3:
        begin  // Lines.Bus1 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        SetBus(1, String(arg));
                    end;
                end;
        end;
        4:
        begin  // Lines.Bus2 read
            Result := Pansichar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := Pansichar(Ansistring(ActiveCircuit[ActiveActor].ActiveCktElement.GetBus(2)));
                end
        end;
        5:
        begin  // Lines.Bus2 write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        SetBus(2, String(arg));
                    end;
                end;
        end;
        6:
        begin  // Lines.LineCode read
            Result := Pansichar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := Pansichar(Ansistring(TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).CondCode));
                end
        end;
        7:
        begin  // Lines.LineCode write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        FetchLineCode(String(arg));
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        8:
        begin  // Lines.Geometry read
            Result := Pansichar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := Pansichar(Ansistring(TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).GeometryCode));
                end
        end;
        9:
        begin  // Lines.Geometry write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Parser[ActiveActor].CmdString := 'geometry=' + arg;
                        Edit(ActiveActor);
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end;
        10:
        begin  // Lines.Spacing read
            Result := Pansichar(Ansistring(''));
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    Result := Pansichar(Ansistring(TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement).SpacingCode));
                end
        end;
        11:
        begin  // Lines.Spacing write
            if ActiveCircuit[ActiveActor] <> NIL then
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Parser[ActiveActor].CmdString := 'spacing=' + arg;
                        Edit(ActiveActor);
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not recognized'));
    end;
end;

//************************Variant type properties*******************************
procedure LinesV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    LineElem: TLineObj;
    Ztemp: complex;
    Factor: Double;
    i,
    j,
    k,
    NValues: Integer;
    cValues: pComplexArray;
    PDouble: ^Double;

begin
    case mode of
        0:
        begin  // Lines/AllNames
            myType := 4;        // String
            setlength(myStrArray, 0);
            mySize := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                begin
                    if Lines.ListSize > 0 then
                    begin
                        LineElem := Lines.First;
                        while LineElem <> NIL do
                        begin
                            WriteStr2Array(LineElem.Name);
                            WriteStr2Array(Char(0));
                            LineElem := Lines.Next;
                        end;
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // Lines.RMatrix read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        setlength(myDBLArray, Sqr(Nphases));
                        k := 0;
                        for i := 1 to NPhases do
                        begin
                            for j := 1 to Nphases do
                            begin
                                if GeometrySpecified or SpacingSpecified then
                                    myDBLArray[k] := Z.GetElement(i, j).Re / Len
                                else
                                    myDBLArray[k] := Z.GetElement(i, j).Re / UnitsConvert;
                                Inc(k);
                            end;
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        2:
        begin  // Lines.RMatrix write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        for i := 1 to NPhases do
                        begin
                            for j := 1 to Nphases do
                            begin
                                PDouble := myPointer;
                                ZTemp := Z.GetElement(i, j);
                                Z.SetElement(i, j, Cmplx(PDouble^, ZTemp.im));
                                Inc(k);
                                inc(Pbyte(myPointer), 8);
                            end;
                        end;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
            end;
            mySize := k;
        end;
        3:
        begin  // Lines.Xmatrix read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        setlength(myDBLArray, Sqr(Nphases));
                        k := 0;
                        for i := 1 to NPhases do
                        begin
                            for j := 1 to Nphases do
                            begin
                                if GeometrySpecified or SpacingSpecified then
                                    myDBLArray[k] := Z.GetElement(i, j).im / Len
                                else
                                    myDBLArray[k] := Z.GetElement(i, j).im / UnitsConvert;
                                Inc(k);
                            end;
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        4:
        begin  // Lines.XMatrix write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        for i := 1 to NPhases do
                        begin
                            for j := 1 to Nphases do
                            begin
                                PDouble := myPointer;
                                ZTemp := Z.GetElement(i, j);
                                Z.SetElement(i, j, Cmplx(Ztemp.re, PDouble^));
                                Inc(k);
                                inc(Pbyte(myPointer), 8);
                            end;
                        end;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
            end;
            mySize := k;
        end;
        5:
        begin  // Lines.CMatrix read
            myType := 2;        // Double
            setlength(myDBLArray, 1);
            myDBLArray[0] := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Factor := TwoPi * BaseFrequency * 1.0e-9 * UnitsConvert;
                        setlength(myDBLArray, Sqr(Nphases));
                        k := 0;
                        for i := 1 to NPhases do
                        begin
                            for j := 1 to Nphases do
                            begin
                                if GeometrySpecified or SpacingSpecified then
                                    myDBLArray[k] := Yc.GetElement(i, j).im / Factor / Len
                                else
                                    myDBLArray[k] := Yc.GetElement(i, j).im / Factor;
                                Inc(k);
                            end;
                        end;
                    end;
                end;
            end;
            myPointer := @(myDBLArray[0]);
            mySize := SizeOf(myDBLArray[0]) * Length(myDBLArray);
        end;
        6:
        begin  // Lines.CMatrix write
            myType := 2;        // Double
            k := 0;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                    begin
                        Factor := TwoPi * BaseFrequency * 1.0e-9;
                        for i := 1 to NPhases do
                        begin
                            for j := 1 to Nphases do
                            begin
                                PDouble := myPointer;
                                Yc.SetElement(i, j, Cmplx(0.0, (PDouble^) * Factor));
                                Inc(k);
                                inc(Pbyte(myPointer), 8);
                            end;
                        end;
                        YprimInvalid[ActiveActor] := TRUE;
                    end;
                end;
            end;
            mySize := k;
        end;
        7:
        begin  // Lines.Yprim read
            myType := 3;        // Complex
            setlength(myCmplxArray, 1);
            myCmplxArray[0] := CZero;
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
                with ActiveCircuit[ActiveActor] do
                    if IsLine(ActiveCircuit[ActiveActor].ActiveCktElement) then
                    begin
                        with TLineObj(ActiveCircuit[ActiveActor].ActiveCktElement) do
                        begin
                            NValues := SQR(Yorder);
                            cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
                            if cValues <> NIL then
                            begin
                                setlength(myCmplxArray, NValues);  // Make variant array
                                for i := 1 to NValues do
                                begin    // Plunk the values in the complex array
                                    myCmplxArray[i - 1] := cValues^[i];
                                end;
                            end;
                        end;
                    end;
                myPointer := @(myCmplxArray[0]);
                mySize := SizeOf(myCmplxArray[0]) * length(myCmplxArray);
            end;
        end;
        8:
        begin  // Lines.Yprim write
            if ActiveCircuit[ActiveActor] <> NIL then
            begin
         {Do Nothing for now}
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
