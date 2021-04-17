unit ImplLines;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TLines = class(TAutoObject, ILines)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Bus1: Widestring; SAFECALL;
        function Get_Bus2: Widestring; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Length: Double; SAFECALL;
        function Get_LineCode: Widestring; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        function Get_R1: Double; SAFECALL;
        function Get_X1: Double; SAFECALL;
        function New(const Name: Widestring): Integer; SAFECALL;
        procedure Set_Bus1(const Value: Widestring); SAFECALL;
        procedure Set_Bus2(const Value: Widestring); SAFECALL;
        procedure Set_Length(Value: Double); SAFECALL;
        procedure Set_LineCode(const Value: Widestring); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        procedure Set_R1(Value: Double); SAFECALL;
        procedure Set_X1(Value: Double); SAFECALL;
        function Get_C0: Double; SAFECALL;
        function Get_C1: Double; SAFECALL;
        function Get_Cmatrix: Olevariant; SAFECALL;
        function Get_R0: Double; SAFECALL;
        function Get_Rmatrix: Olevariant; SAFECALL;
        function Get_X0: Double; SAFECALL;
        function Get_Xmatrix: Olevariant; SAFECALL;
        procedure Set_C0(Value: Double); SAFECALL;
        procedure Set_C1(Value: Double); SAFECALL;
        procedure Set_Cmatrix(Value: Olevariant); SAFECALL;
        procedure Set_R0(Value: Double); SAFECALL;
        procedure Set_Rmatrix(Value: Olevariant); SAFECALL;
        procedure Set_X0(Value: Double); SAFECALL;
        procedure Set_Xmatrix(Value: Olevariant); SAFECALL;
        function Get_EmergAmps: Double; SAFECALL;
        function Get_NormAmps: Double; SAFECALL;
        procedure Set_EmergAmps(Value: Double); SAFECALL;
        procedure Set_NormAmps(Value: Double); SAFECALL;
        function Get_Geometry: Widestring; SAFECALL;
        procedure Set_Geometry(const Value: Widestring); SAFECALL;
        function Get_Rg: Double; SAFECALL;
        function Get_Rho: Double; SAFECALL;
        function Get_Xg: Double; SAFECALL;
        procedure Set_Rg(Value: Double); SAFECALL;
        procedure Set_Rho(Value: Double); SAFECALL;
        procedure Set_Xg(Value: Double); SAFECALL;
        function Get_Yprim: Olevariant; SAFECALL;
        procedure Set_Yprim(Value: Olevariant); SAFECALL;
        function Get_NumCust: Integer; SAFECALL;
        function Get_TotalCust: Integer; SAFECALL;
        function Get_Parent: Integer; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_Spacing: Widestring; SAFECALL;
        procedure Set_Spacing(const Value: Widestring); SAFECALL;
        function Get_Units: Integer; SAFECALL;
        procedure Set_Units(Value: Integer); SAFECALL;

    { Protected declarations }
    end;

implementation

uses
    ComServ,
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

function TLines.Get_AllNames: Olevariant;
var
    LineElem: TLineObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if Lines.ListSize > 0 then
            begin
                VarArrayRedim(Result, Lines.ListSize - 1);
                k := 0;
                LineElem := Lines.First;
                while LineElem <> NIL do
                begin
                    Result[k] := LineElem.Name;
                    Inc(k);
                    LineElem := Lines.Next;
                end;
            end;

end;

function TLines.Get_Bus1: Widestring;
begin
    Result := '';
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := ActiveCircuit.ActiveCktElement.GetBus(1);
        end

end;

function TLines.Get_Bus2: Widestring;
begin
    Result := '';
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := ActiveCircuit.ActiveCktElement.GetBus(2);
        end
end;

function TLines.Get_First: Integer;
var
    pLine: TLineObj;

begin

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

function TLines.Get_Length: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).Len;
        end
end;

function TLines.Get_LineCode: Widestring;
begin
    Result := '';
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).CondCode;
        end

end;

function TLines.Get_Name: Widestring;
var
    pLine: TDSSCktElement;

begin
    Result := '';  // signify no name
    if ActiveCircuit <> NIL then
    begin
        pLine := ActiveCircuit.ActiveCktElement;
        if pLine <> NIL then
        begin
            Result := pLine.Name;
        end;
    end;

end;

function TLines.Get_Next: Integer;
var
    pLine: TLineObj;

begin

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

function TLines.Get_Phases: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := ActiveCircuit.ActiveCktElement.Nphases;
        end

end;

function TLines.Get_R1: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).R1;
        end

end;

function TLines.Get_X1: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).X1;
        end

end;

function TLines.New(const Name: Widestring): Integer;
begin
    Result := AddObject('line', Name);    // Returns handle to object
end;


function TLines.Get_Units: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).LengthUnits;
        end

end;


procedure TLines.Set_Bus1(const Value: Widestring);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                SetBus(1, Value);
            end;
        end;
end;

procedure TLines.Set_Bus2(const Value: Widestring);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                SetBus(2, Value);
            end;
        end;
end;

procedure TLines.Set_Length(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Len := Value;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_LineCode(const Value: Widestring);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                FetchLineCode(Value);
                YprimInvalid := TRUE;
            end;
        end;

end;

procedure TLines.Set_Name(const Value: Widestring);
var
    activesave: Integer;
    pLine: TLineObj;
    S: String;
    Found: Boolean;
begin


    if ActiveCircuit <> NIL then
    begin      // Search list of Lines in active circuit for name
        with ActiveCircuit.Lines do
        begin
            S := Value;  // Convert to Pascal String
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

procedure TLines.Set_Phases(Value: Integer);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Nphases := Value;
                YprimInvalid := TRUE;
            end;
        end;

end;

procedure TLines.Set_R1(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                R1 := Value;
                SymComponentsChanged := TRUE;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_X1(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                X1 := Value;
                SymComponentsChanged := TRUE;
                YprimInvalid := TRUE;
            end;
        end;
end;

function TLines.Get_C0: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Result := C0 / UnitsConvert * 1.0e9;
            end

end;

function TLines.Get_C1: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Result := C1 / UnitsConvert * 1.0e9;
            end;

end;

function TLines.Get_Cmatrix: Olevariant;
var
    i, j, k: Integer;
    Factor: Double;

begin

    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Factor := TwoPi * BaseFrequency * 1.0e-9 * UnitsConvert;  // corrected 2.9.2018 RCD
                Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                k := 0;
                for i := 1 to NPhases do
                    for j := 1 to Nphases do
                    begin
                        Result[k] := Yc.GetElement(i, j).im / Factor;
                        Inc(k);
                    end;
            end;
        end;

end;

function TLines.Get_R0: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Result := R0 / UnitsConvert;
            end;

end;

function TLines.Get_Rmatrix: Olevariant;
var
    i, j, k: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                k := 0;
                for i := 1 to NPhases do
                    for j := 1 to Nphases do
                    begin
                        Result[k] := Z.GetElement(i, j).Re / UnitsConvert;
                        Inc(k);
                    end;
            end;
        end;
end;

function TLines.Get_X0: Double;
begin

    Result := 0.0;

    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Result := X0 / UnitsConvert;
            end;

end;

function TLines.Get_Xmatrix: Olevariant;
var
    i, j, k: Integer;
begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Result := VarArrayCreate([0, Sqr(Nphases) - 1], varDouble);
                k := 0;
                for i := 1 to NPhases do
                    for j := 1 to Nphases do
                    begin
                        Result[k] := Z.GetElement(i, j).im / UnitsConvert;
                        Inc(k);
                    end;
            end;
        end;
end;

procedure TLines.Set_C0(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                C0 := Value * 1.0e-9;
                SymComponentsChanged := TRUE;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_C1(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                C1 := Value * 1.0e-9;
                SymComponentsChanged := TRUE;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_Cmatrix(Value: Olevariant);
var
    i, j, k: Integer;
    Factor: Double;
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Factor := TwoPi * BaseFrequency * 1.0e-9;
                k := VarArrayLowBound(Value, 1);
                for i := 1 to NPhases do
                    for j := 1 to Nphases do
                    begin
                        Yc.SetElement(i, j, Cmplx(0.0, Value[k] * Factor));
                        Inc(k);
                    end;
                YprimInvalid := TRUE;
            end;
        end;

end;

procedure TLines.Set_R0(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                R0 := Value;
                SymComponentsChanged := TRUE;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_Rmatrix(Value: Olevariant);
var
    i, j, k: Integer;
    Ztemp: complex;
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                k := VarArrayLowBound(Value, 1);
                for i := 1 to NPhases do
                    for j := 1 to Nphases do
                    begin
                        ZTemp := Z.GetElement(i, j);
                        Z.SetElement(i, j, Cmplx(Value[k], ZTemp.im));
                        Inc(k);
                    end;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_X0(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                X0 := Value;
                SymComponentsChanged := TRUE;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_Xmatrix(Value: Olevariant);

var
    i, j, k: Integer;
    Ztemp: complex;
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                k := VarArrayLowBound(Value, 1);
                for i := 1 to NPhases do
                    for j := 1 to Nphases do
                    begin
                        ZTemp := Z.GetElement(i, j);
                        Z.SetElement(i, j, Cmplx(Ztemp.re, Value[k]));
                        Inc(k);
                    end;
                YprimInvalid := TRUE;

            end;
        end;
end;

function TLines.Get_EmergAmps: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).EmergAmps;
        end

end;

function TLines.Get_NormAmps: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps;
        end
end;

procedure TLines.Set_EmergAmps(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                EmergAmps := Value;
            end;
        end;
end;

procedure TLines.Set_NormAmps(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                NormAmps := Value;
            end;
        end;
end;

function TLines.Get_Geometry: Widestring;
begin
    Result := '';
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).GeometryCode;
        end
end;

procedure TLines.Set_Geometry(const Value: Widestring);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Parser.CmdString := 'geometry=' + Value;
                Edit;
                YprimInvalid := TRUE;
            end;
        end;
end;

function TLines.Get_Rg: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).Rg;
        end
end;

function TLines.Get_Rho: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).rho;
        end
end;

function TLines.Get_Xg: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).Xg;
        end
end;

procedure TLines.Set_Rg(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Parser.CmdString := Format('rg=%.7g', [Value]);
                Edit;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_Rho(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Parser.CmdString := Format('rho=%.7g', [Value]);
                Edit;
                YprimInvalid := TRUE;
            end;
        end;
end;

procedure TLines.Set_Units(Value: Integer);
{
 This code assumes the present value of line units is NONE.
 The Set functions in this interface all set values in this length unit.
}
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                if Value < dssLineUnitsMaxnum then
                begin
                    Parser.CmdString := Format('units=%s', [LineUnitsStr(Value)]);
                    Edit;
                    YprimInvalid := TRUE;
                end
                else
                    DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.', 183);

            end;
        end;
end;


procedure TLines.Set_Xg(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Parser.CmdString := Format('xg=%.7g', [Value]);
                Edit;
                YprimInvalid := TRUE;
            end;
        end;
end;

function TLines.Get_Yprim: Olevariant;

{ Return the YPrim matrix for this element }

var
    iV: Integer;
    i: Integer;
    NValues: Integer;
    cValues: pComplexArray;

begin
    if ActiveCircuit = NIL then
    begin
        Result := VarArrayCreate([0, 0], varDouble);
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
                        Result := VarArrayCreate([0, 0], varDouble);  // just return null array
                        Exit;  // Get outta here
                    end;
                    Result := VarArrayCreate([0, 2 * NValues - 1], varDouble);  // Make variant array
                    iV := 0;

                    for i := 1 to NValues do
                    begin    // Plunk the values in the variant array
                        Result[iV] := cValues^[i].re;
                        Inc(iV);
                        Result[iV] := cValues^[i].im;
                        Inc(iV);
                    end;
                end
            else
                Result := VarArrayCreate([0, 0], varDouble);  // just return null array

end;

procedure TLines.Set_Yprim(Value: Olevariant);
begin
    if ActiveCircuit <> NIL then
    begin
       {Do Nothing for now}
    end;
end;

function TLines.Get_NumCust: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchNumCustomers;
        end
end;

function TLines.Get_TotalCust: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchTotalCustomers;
        end
end;

function TLines.Get_Parent: Integer;

{ Sets the Active Line to the immediately upline Line obj, if any}
{ Returns line index  or 0 if it fails or no more lines}

var
    pLine: TLineObj;

begin

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

function TLines.Get_Count: Integer;
begin
    if Assigned(Activecircuit) then
        Result := ActiveCircuit.Lines.ListSize;
end;

function TLines.Get_Spacing: Widestring;
begin
    Result := '';
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            Result := TLineObj(ActiveCircuit.ActiveCktElement).SpacingCode;
        end
end;

procedure TLines.Set_Spacing(const Value: Widestring);
begin
    if ActiveCircuit <> NIL then
        if IsLine(ActiveCircuit.ActiveCktElement) then
        begin
            with TLineObj(ActiveCircuit.ActiveCktElement) do
            begin
                Parser.CmdString := 'spacing=' + Value;
                Edit;
                YprimInvalid := TRUE;
            end;
        end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TLines, Class_Lines,
        ciInternal, tmApartment);
end.
