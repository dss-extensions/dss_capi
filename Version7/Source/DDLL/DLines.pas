unit DLines;

interface

function LinesI(mode: Longint; arg: Longint): Longint; CDECL;
function LinesF(mode: Longint; arg: Double): Double; CDECL;
function LinesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;
procedure LinesV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    Line,
    DSSClassDefs,
    DSSGlobals,
    CktElement,
    uComplex,
    ExecHelper,
    dialogs,
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
            if ActiveCircuit <> NIL then
            begin
                pLine := ActiveCircuit.Lines.First;
                if pLine <> NIL then
                begin
                    repeat
                        if pLine.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pLine;
                            Result := 1;
                        end
                        else
                            pLine := ActiveCircuit.Lines.Next;
                    until (Result = 1) or (pLine = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        1:
        begin  // Lines.Next
            Result := 0;
            if ActiveCircuit <> NIL then
            begin
                pLine := ActiveCircuit.Lines.Next;
                if pLine <> NIL then
                begin
                    repeat
                        if pLine.Enabled then
                        begin
                            ActiveCircuit.ActiveCktElement := pLine;
                            Result := ActiveCircuit.Lines.ActiveIndex;
                        end
                        else
                            pLine := ActiveCircuit.Lines.Next;
                    until (Result > 0) or (pLine = NIL);
                end
                else
                    Result := 0;  // signify no more
            end;
        end;
        2:
        begin  // Lines.Phases read
            Result := 0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := ActiveCircuit.ActiveCktElement.Nphases;
                end
        end;
        3:
        begin  // Lines.Phases write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Nphases := arg;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        4:
        begin  // Lines.NumCust
            Result := 0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchNumCustomers;
                end
        end;
        5:
        begin  // Lines.Parent
            Result := 0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    pLine := TLineObj(ActiveCircuit.ActiveCktElement);
                    if pLine.ParentPDelement <> NIL then
                    begin
                        if (pLine.ParentPDelement.Enabled) and (IsLine(pLine.ParentPDelement)) then
                        begin
                            ActiveCircuit.ActiveCktElement := pLine.ParentPDElement;
                            Result := ActiveCircuit.Lines.ActiveIndex;
                        end;
                    end;
                end;
        end;
        6:
        begin  // Lines.Count
            if Assigned(Activecircuit) then
                Result := ActiveCircuit.Lines.ListSize;
        end;
        7:
        begin  // Lines.Units read
            Result := 0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).LengthUnits;
                end
        end;
        8:
        begin  // Line.Units write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        if arg < 9 then
                        begin
                            Parser.CmdString := Format('units=%s', [LineUnitsStr(arg)]);
                            Edit;
                            YprimInvalid := TRUE;
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
begin
    Result := 0.0;
    case mode of
        0:
        begin  // Lines.Length read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).Len;
                end
        end;
        1:
        begin  // Lines.Length write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Len := arg;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        2:
        begin  // Lines.R1 read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).R1;
                end
        end;
        3:
        begin  // Lines.R1 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        R1 := arg;
                        SymComponentsChanged := TRUE;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        4:
        begin  // Lines.X1 read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).X1;
                end
        end;
        5:
        begin  // Lines.X1 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        X1 := arg;
                        SymComponentsChanged := TRUE;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        6:
        begin  // Lines.R0 read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Result := R0 / UnitsConvert;
                    end;
        end;
        7:
        begin  // Lines.R0 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        R0 := arg;
                        SymComponentsChanged := TRUE;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        8:
        begin  // Lines.X0 read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Result := X0 / UnitsConvert;
                    end;
        end;
        9:
        begin  // Lines.X0 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        X0 := arg;
                        SymComponentsChanged := TRUE;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        10:
        begin  // Lines.C1 read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Result := C1 / UnitsConvert * 1.0e9;
                    end;
        end;
        11:
        begin  // Lines.C1 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        C1 := arg * 1.0e-9;
                        SymComponentsChanged := TRUE;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        12:
        begin // Lines.C0 read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Result := C0 / UnitsConvert * 1.0e9;
                    end
        end;
        13:
        begin  // Line.C0 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        C0 := arg * 1.0e-9;
                        SymComponentsChanged := TRUE;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        14:
        begin  // Lines.NormAmps read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps;
                end
        end;
        15:
        begin  // Lines.NormAmps write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        NormAmps := arg;
                    end;
                end;
        end;
        16:
        begin  // Lines.EmergAmps read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).EmergAmps;
                end
        end;
        17:
        begin  // Lines.EmergAmps write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        EmergAmps := arg;
                    end;
                end;
        end;
        18:
        begin  // Lines.Rg read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).Rg;
                end
        end;
        19:
        begin  // Lines.Rg write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Parser.CmdString := Format('rg=%.7g', [arg]);
                        Edit;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        20:
        begin  // Lines.Xg read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).Xg;
                end
        end;
        21:
        begin  // Lines.Xg write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Parser.CmdString := Format('xg=%.7g', [arg]);
                        Edit;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        22:
        begin  // Lines.Rho read
            Result := 0.0;
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := TLineObj(ActiveCircuit.ActiveCktElement).rho;
                end
        end;
        23:
        begin  // Lines.Rho write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Parser.CmdString := Format('rho=%.7g', [arg]);
                        Edit;
                        YprimInvalid := TRUE;
                    end;
                end;
        end
    else
        Result := -1.0;
    end;
end;

//******************************String type properties****************************
function LinesS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    pLine: TDSSCktElement;
    activesave: Integer;
    S: String;
    Found: Boolean;

begin
    Result := pAnsiChar(Ansistring(''));
    case mode of
        0:
        begin  // Lines.Name read
            Result := pAnsiChar(Ansistring(''));  // signify no name
            if ActiveCircuit <> NIL then
            begin
                pLine := ActiveCircuit.ActiveCktElement;
                if pLine <> NIL then
                begin
                    Result := pAnsiChar(Ansistring(pLine.Name));
                end;
            end;
        end;
        1:
        begin  // Lines.Name write
            if ActiveCircuit <> NIL then
            begin      // Search list of Lines in active circuit for name
                with ActiveCircuit.Lines do
                begin
                    S := String(arg);  // Convert to Pascal String
                    Found := FALSE;
                    ActiveSave := ActiveIndex;
                    pLine := First;
                    while pLine <> NIL do
                    begin
                        if (CompareText(pLine.Name, S) = 0) then
                        begin
                            ActiveCircuit.ActiveCktElement := pLine;
                            Found := TRUE;
                            Break;
                        end;
                        pLine := Next;
                    end;
                    if not Found then
                    begin
                        DoSimpleMsg('Line "' + S + '" Not Found in Active Circuit.', 5008);
                        pLine := Get(ActiveSave);    // Restore active Line
                        ActiveCircuit.ActiveCktElement := pLine;
                    end;
                end;
            end;
        end;
        2:
        begin  // Lines.Bus1 read
            Result := String(Ansistring(''));
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := pAnsiChar(Ansistring(ActiveCircuit.ActiveCktElement.GetBus(1)));
                end
        end;
        3:
        begin  // Lines.Bus1 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        SetBus(1, String(arg));
                    end;
                end;
        end;
        4:
        begin  // Lines.Bus2 read
            Result := String(Ansistring(''));
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := pAnsiChar(Ansistring(ActiveCircuit.ActiveCktElement.GetBus(2)));
                end
        end;
        5:
        begin  // Lines.Bus2 write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        SetBus(2, String(arg));
                    end;
                end;
        end;
        6:
        begin  // Lines.LineCode read
            Result := String(Ansistring(''));
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := pAnsiChar(Ansistring(TLineObj(ActiveCircuit.ActiveCktElement).CondCode));
                end
        end;
        7:
        begin  // Lines.LineCode write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        FetchLineCode(String(arg));
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        8:
        begin  // Lines.Geometry read
            Result := String(Ansistring(''));
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := pAnsiChar(Ansistring(TLineObj(ActiveCircuit.ActiveCktElement).GeometryCode));
                end
        end;
        9:
        begin  // Lines.Geometry write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Parser.CmdString := 'geometry=' + String(arg);
                        Edit;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        10:
        begin  // Lines.Spacing read
            Result := String(Ansistring(''));
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    Result := pAnsiChar(Ansistring(TLineObj(ActiveCircuit.ActiveCktElement).SpacingCode));
                end
        end;
        11:
        begin  // Lines.Spacing write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Parser.CmdString := 'spacing=' + String(arg);
                        Edit;
                        YprimInvalid := TRUE;
                    end;
                end;
        end
    else
        Result := String(Ansistring('Error, parameter not recognized'));
    end;
end;

//************************Variant type properties*******************************
procedure LinesV(mode: Longint; out arg: Variant); CDECL;

var
    LineElem: TLineObj;
    i, j, k: Integer;
    Ztemp: complex;
    Factor: Double;
    iV: Integer;
    NValues: Integer;
    cValues: pComplexArray;

begin
    case mode of
        0:
        begin  // Lines/AllNames
            arg := VarArrayCreate([0, 0], varOleStr);
            arg[0] := 'NONE';
            if ActiveCircuit <> NIL then
                with ActiveCircuit do
                    if Lines.ListSize > 0 then
                    begin
                        VarArrayRedim(arg, Lines.ListSize - 1);
                        k := 0;
                        LineElem := Lines.First;
                        while LineElem <> NIL do
                        begin
                            arg[k] := LineElem.Name;
                            Inc(k);
                            LineElem := Lines.Next;
                        end;
                    end;
        end;
        1:
        begin  // Lines.RMatrix read
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        arg := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                        k := 0;
                        for i := 1 to NPhases do
                            for j := 1 to Nphases do
                            begin
                                arg[k] := Z.GetElement(i, j).Re / UnitsConvert;
                                Inc(k);
                            end;
                    end;
                end;
        end;
        2:
        begin  // Lines.RMatrix write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        k := VarArrayLowBound(arg, 1);
                        for i := 1 to NPhases do
                            for j := 1 to Nphases do
                            begin
                                ZTemp := Z.GetElement(i, j);
                                Z.SetElement(i, j, Cmplx(arg[k], ZTemp.im));
                                Inc(k);
                            end;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        3:
        begin  // Lines.Xmatrix read
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        arg := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                        k := 0;
                        for i := 1 to NPhases do
                            for j := 1 to Nphases do
                            begin
                                arg[k] := Z.GetElement(i, j).im / UnitsConvert;
                                Inc(k);
                            end;
                    end;
                end;
        end;
        4:
        begin  // Lines.XMatrix write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        k := VarArrayLowBound(arg, 1);
                        for i := 1 to NPhases do
                            for j := 1 to Nphases do
                            begin
                                ZTemp := Z.GetElement(i, j);
                                Z.SetElement(i, j, Cmplx(Ztemp.re, arg[k]));
                                Inc(k);
                            end;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        5:
        begin  // Lines.CMatrix read
            arg := VarArrayCreate([0, 0], varDouble);
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Factor := TwoPi * BaseFrequency * 1.0e-9 * UnitsConvert;  // corrected 2.9.2018 RCD
                        arg := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                        k := 0;
                        for i := 1 to NPhases do
                            for j := 1 to Nphases do
                            begin
                                arg[k] := Yc.GetElement(i, j).im / Factor;
                                Inc(k);
                            end;
                    end;
                end;
        end;
        6:
        begin  // Lines.CMatrix write
            if ActiveCircuit <> NIL then
                if IsLine(ActiveCircuit.ActiveCktElement) then
                begin
                    with TLineObj(ActiveCircuit.ActiveCktElement) do
                    begin
                        Factor := TwoPi * BaseFrequency * 1.0e-9;
                        k := VarArrayLowBound(arg, 1);
                        for i := 1 to NPhases do
                            for j := 1 to Nphases do
                            begin
                                Yc.SetElement(i, j, Cmplx(0.0, arg[k] * Factor));
                                Inc(k);
                            end;
                        YprimInvalid := TRUE;
                    end;
                end;
        end;
        7:
        begin  // Lines.Yprim read
            if ActiveCircuit = NIL then
            begin
                arg := VarArrayCreate([0, 0], varDouble);
            end
            else
                with ActiveCircuit do
                    if IsLine(ActiveCircuit.ActiveCktElement) then
                        with TLineObj(ActiveCircuit.ActiveCktElement) do
                        begin
                            NValues := SQR(Yorder);
                            cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
                            if cValues = NIL then
                            begin   // check for unassigned array
                                arg := VarArrayCreate([0, 0], varDouble);  // just return null array
                                Exit;  // Get outta here
                            end;
                            arg := VarArrayCreate([0, 2 * NValues - 1], varDouble);  // Make variant array
                            iV := 0;
                            for i := 1 to NValues do
                            begin    // Plunk the values in the variant array
                                arg[iV] := cValues^[i].re;
                                Inc(iV);
                                arg[iV] := cValues^[i].im;
                                Inc(iV);
                            end;
                        end
                    else
                        arg := VarArrayCreate([0, 0], varDouble);  // just return null array
        end;
        8:
        begin  // Lines.Yprim write
            if ActiveCircuit <> NIL then
            begin
       {Do Nothing for now}
            end;
        end
    else
        arg[0] := 'Error, parameter not recognized';
    end;
end;

end.
