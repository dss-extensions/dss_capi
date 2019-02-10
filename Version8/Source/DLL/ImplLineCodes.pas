unit ImplLineCodes;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl,
    LineCode;

type
    TLineCodes = class(TAutoObject, ILineCodes)
    PROTECTED
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_IsZ1Z0: Wordbool; SAFECALL;
        function Get_Units: Integer; SAFECALL;
        procedure Set_Units(Value: Integer); SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;
        function Get_R1: Double; SAFECALL;
        procedure Set_R1(Value: Double); SAFECALL;
        function Get_X1: Double; SAFECALL;
        procedure Set_X1(Value: Double); SAFECALL;
        function Get_R0: Double; SAFECALL;
        function Get_X0: Double; SAFECALL;
        procedure Set_R0(Value: Double); SAFECALL;
        procedure Set_X0(Value: Double); SAFECALL;
        function Get_C0: Double; SAFECALL;
        function Get_C1: Double; SAFECALL;
        procedure Set_C0(Value: Double); SAFECALL;
        procedure Set_C1(Value: Double); SAFECALL;
        function Get_Cmatrix: Olevariant; SAFECALL;
        function Get_Rmatrix: Olevariant; SAFECALL;
        function Get_Xmatrix: Olevariant; SAFECALL;
        procedure Set_Cmatrix(Value: Olevariant); SAFECALL;
        procedure Set_Rmatrix(Value: Olevariant); SAFECALL;
        procedure Set_Xmatrix(Value: Olevariant); SAFECALL;
        function Get_NormAmps: Double; SAFECALL;
        procedure Set_NormAmps(Value: Double); SAFECALL;
        function Get_EmergAmps: Double; SAFECALL;
        procedure Set_EmergAmps(Value: Double); SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;

    end;

implementation

uses
    ComServ,
    sysutils,
    DSSGlobals,
    LineUnits,
    ParserDel,
    Variants,
    Ucomplex;

function TLineCodes.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := LineCodeClass.ElementCount;
end;

function TLineCodes.Get_First: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := LineCodeClass.First;
end;

function TLineCodes.Get_Next: Integer;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := LineCodeClass.Next;
end;

function TLineCodes.Get_Name: Widestring;

var
    pLineCode: TLineCodeObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        if pLineCode <> NIL then
        begin
            Result := pLineCode.Name;
        end;
    end;

end;

procedure TLineCodes.Set_Name(const Value: Widestring);

// set LineCode active by name

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if not LineCodeClass.SetActive(Value) then
            DoSimpleMsg('LineCode "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;

function TLineCodes.Get_IsZ1Z0: Wordbool;
var
    pLineCode: TLineCodeObj;

begin
    Result := TRUE;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        if pLineCode <> NIL then
        begin
            Result := pLineCode.SymComponentsModel;
        end;
    end;
end;

function TLineCodes.Get_Units: Integer;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.Units;
    end
end;

procedure TLineCodes.Set_Units(Value: Integer);
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            if Value < dssLineUnitsMaxnum then
            begin
                Parser[ActiveActor].CmdString := Format('units=%s', [LineUnitsStr(Value)]);
                Edit(ActiveActor);
            end
            else
                DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.', 183);

        end;
    end;
end;

function TLineCodes.Get_Phases: Integer;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.FNPhases;
    end

end;

procedure TLineCodes.Set_Phases(Value: Integer);
var
    pLineCode: TLineCodeObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        pLineCode.NumPhases := Value;   // use property value to force reallocations
    end

end;

function TLineCodes.Get_R1: Double;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.R1;
    end

end;

procedure TLineCodes.Set_R1(Value: Double);
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser[ActiveActor].CmdString := Format('R1=%g', [Value]);
            Edit(ActiveActor);
        end;
    end;

end;

function TLineCodes.Get_X1: Double;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.X1;
    end

end;

procedure TLineCodes.Set_X1(Value: Double);
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser[ActiveActor].CmdString := Format('X1=%g', [Value]);
            Edit(ActiveActor);
        end;
    end;

end;

function TLineCodes.Get_R0: Double;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.R0;
    end

end;

function TLineCodes.Get_X0: Double;

var
    pLineCode: TLineCodeObj;

begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.X0;
    end

end;

procedure TLineCodes.Set_R0(Value: Double);

var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser[ActiveActor].CmdString := Format('R0=%g', [Value]);
            Edit(ActiveActor);
        end;
    end;

end;

procedure TLineCodes.Set_X0(Value: Double);

var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser[ActiveActor].CmdString := Format('X0=%g', [Value]);
            Edit(ActiveActor);
        end;
    end;

end;

function TLineCodes.Get_C0: Double;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.C0;
    end

end;

function TLineCodes.Get_C1: Double;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.C1;
    end

end;

procedure TLineCodes.Set_C0(Value: Double);

var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser[ActiveActor].CmdString := Format('C0=%g', [Value]);
            Edit(ActiveActor);
        end;
    end;

end;

procedure TLineCodes.Set_C1(Value: Double);

var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser[ActiveActor].CmdString := Format('C1=%g', [Value]);
            Edit(ActiveActor);
        end;
    end;

end;

function TLineCodes.Get_Cmatrix: Olevariant;

var
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Factor: Double;

begin

    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Factor := (TwoPi * BaseFrequency * 1.0e-9);
            Result := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
            k := 0;
            for i := 1 to FNPhases do
                for j := 1 to FNphases do
                begin
                    Result[k] := YC.GetElement(i, j).im / Factor;
                    Inc(k);
                end;
        end;
    end;

end;

function TLineCodes.Get_Rmatrix: Olevariant;

var
    i, j, k: Integer;
    pLineCode: TLineCodeObj;

begin

    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Result := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
            k := 0;
            for i := 1 to FNPhases do
                for j := 1 to FNphases do
                begin
                    Result[k] := Z.GetElement(i, j).re;
                    Inc(k);
                end;
        end;
    end;

end;

function TLineCodes.Get_Xmatrix: Olevariant;
var
    i, j, k: Integer;
    pLineCode: TLineCodeObj;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Result := VarArrayCreate([0, Sqr(FNphases) - 1], varDouble);
            k := 0;
            for i := 1 to FNPhases do
                for j := 1 to FNphases do
                begin
                    Result[k] := Z.GetElement(i, j).im;
                    Inc(k);
                end;
        end;
    end;

end;

procedure TLineCodes.Set_Cmatrix(Value: Olevariant);
var
    i, j, k: Integer;
    Factor: Double;
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Factor := TwoPi * BaseFrequency * 1.0e-9;
            k := VarArrayLowBound(Value, 1);
            for i := 1 to FNPhases do
                for j := 1 to FNphases do
                begin
                    Yc.SetElement(i, j, Cmplx(0.0, Value[k] * Factor));
                    Inc(k);
                end;
        end;
    end;

end;

procedure TLineCodes.Set_Rmatrix(Value: Olevariant);

var
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Ztemp: complex;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            k := VarArrayLowBound(Value, 1);
            for i := 1 to FNPhases do
                for j := 1 to FNphases do
                begin
                    ZTemp := Z.GetElement(i, j);
                    Z.SetElement(i, j, Cmplx(Value[k], ZTemp.im));
                    Inc(k);
                end;
        end;
    end;

end;

procedure TLineCodes.Set_Xmatrix(Value: Olevariant);

var
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Ztemp: complex;

begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            k := VarArrayLowBound(Value, 1);
            for i := 1 to FNPhases do
                for j := 1 to FNphases do
                begin
                    ZTemp := Z.GetElement(i, j);
                    Z.SetElement(i, j, Cmplx(ZTemp.re, Value[k]));
                    Inc(k);
                end;
        end;
    end;

end;

function TLineCodes.Get_NormAmps: Double;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.NormAmps;
    end

end;

procedure TLineCodes.Set_NormAmps(Value: Double);
var
    pLineCode: TLineCodeObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        pLineCode.NormAmps := Value;
    end

end;

function TLineCodes.Get_EmergAmps: Double;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.EmergAmps;
    end

end;

procedure TLineCodes.Set_EmergAmps(Value: Double);
var
    pLineCode: TLineCodeObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        pLineCode.EmergAmps := Value;
    end

end;

function TLineCodes.Get_AllNames: Olevariant;
var
    LineCodeElem: TLineCodeObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if LineCodeClass.ElementList.ListSize > 0 then
            begin
                VarArrayRedim(Result, LineCodeClass.ElementList.ListSize - 1);
                k := 0;
                LineCodeElem := LineCodeClass.ElementList.First;
                while LineCodeElem <> NIL do
                begin
                    Result[k] := LineCodeElem.Name;
                    Inc(k);
                    LineCodeElem := LineCodeClass.ElementList.Next;
                end;
            end;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TLineCodes, Class_LineCodes,
        ciInternal, tmApartment);
end.
