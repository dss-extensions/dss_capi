unit CAPI_LineCodes;

{$inline on}

interface

uses
    CAPI_Utils,
    LineCode;

function LineCodes_Get_Count(): Integer; CDECL;
function LineCodes_Get_First(): Integer; CDECL;
function LineCodes_Get_Next(): Integer; CDECL;
function LineCodes_Get_Name(): PAnsiChar; CDECL;
procedure LineCodes_Set_Name(const Value: PAnsiChar); CDECL;
function LineCodes_Get_IsZ1Z0(): Boolean; CDECL;
function LineCodes_Get_Units(): Integer; CDECL;
procedure LineCodes_Set_Units(Value: Integer); CDECL;
function LineCodes_Get_Phases(): Integer; CDECL;
procedure LineCodes_Set_Phases(Value: Integer); CDECL;
function LineCodes_Get_R1(): Double; CDECL;
procedure LineCodes_Set_R1(Value: Double); CDECL;
function LineCodes_Get_X1(): Double; CDECL;
procedure LineCodes_Set_X1(Value: Double); CDECL;
function LineCodes_Get_R0(): Double; CDECL;
function LineCodes_Get_X0(): Double; CDECL;
procedure LineCodes_Set_R0(Value: Double); CDECL;
procedure LineCodes_Set_X0(Value: Double); CDECL;
function LineCodes_Get_C0(): Double; CDECL;
function LineCodes_Get_C1(): Double; CDECL;
procedure LineCodes_Set_C0(Value: Double); CDECL;
procedure LineCodes_Set_C1(Value: Double); CDECL;
procedure LineCodes_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LineCodes_Get_Cmatrix_GR(); CDECL;
procedure LineCodes_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LineCodes_Get_Rmatrix_GR(); CDECL;
procedure LineCodes_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LineCodes_Get_Xmatrix_GR(); CDECL;
procedure LineCodes_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LineCodes_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LineCodes_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function LineCodes_Get_NormAmps(): Double; CDECL;
procedure LineCodes_Set_NormAmps(Value: Double); CDECL;
function LineCodes_Get_EmergAmps(): Double; CDECL;
procedure LineCodes_Set_EmergAmps(Value: Double); CDECL;
procedure LineCodes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;

function LineCodes_Get_idx(): Integer; CDECL;
procedure LineCodes_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    ParserDel,
    Ucomplex;

function LineCodes_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := LineCodeClass.ElementCount;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := LineCodeClass.First;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := LineCodeClass.Next;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Name_AnsiString(): Ansistring; inline;
var
    pLineCode: TLineCodeObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        if pLineCode <> NIL then
        begin
            Result := pLineCode.Name;
        end;
    end;

end;

function LineCodes_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(LineCodes_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if not LineCodeClass.SetActive(Value) then
            DoSimpleMsg('LineCode "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_IsZ1Z0(): Boolean; CDECL;
var
    pLineCode: TLineCodeObj;

begin
    Result := TRUE;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        if pLineCode <> NIL then
        begin
            Result := pLineCode.SymComponentsModel;
        end;
    end;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Units(): Integer; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.Units;
    end
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Units(Value: Integer); CDECL;
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            if Value < dssLineUnitsMaxnum then
            begin
                Parser.CmdString := Format('units=%s', [LineUnitsStr(Value)]);
                Edit;
            end
            else
                DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.', 183);

        end;
    end;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Phases(): Integer; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.FNPhases;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Phases(Value: Integer); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        pLineCode.NumPhases := Value;   // use property value to force reallocations
    end

end;
//------------------------------------------------------------------------------
function LineCodes_Get_R1(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.R1;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_R1(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser.CmdString := Format('R1=%g', [Value]);
            Edit;
        end;
    end;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_X1(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.X1;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_X1(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser.CmdString := Format('X1=%g', [Value]);
            Edit;
        end;
    end;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_R0(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.R0;
    end

end;
//------------------------------------------------------------------------------
function LineCodes_Get_X0(): Double; CDECL;
var
    pLineCode: TLineCodeObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.X0;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_R0(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser.CmdString := Format('R0=%g', [Value]);
            Edit;
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_X0(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser.CmdString := Format('X0=%g', [Value]);
            Edit;
        end;
    end;

end;
//------------------------------------------------------------------------------
function LineCodes_Get_C0(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.C0;
    end

end;
//------------------------------------------------------------------------------
function LineCodes_Get_C1(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.C1;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_C0(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser.CmdString := Format('C0=%g', [Value]);
            Edit;
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_C1(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Parser.CmdString := Format('C1=%g', [Value]);
            Edit;
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Factor: Double;

begin

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Factor := (TwoPi * BaseFrequency * 1.0e-9);
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(FNphases) - 1) + 1);
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

procedure LineCodes_Get_Cmatrix_GR(); CDECL;
// Same as LineCodes_Get_Cmatrix but uses global result (GR) pointers
begin
    LineCodes_Get_Cmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineCodes_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;

begin

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(FNphases) - 1) + 1);
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

procedure LineCodes_Get_Rmatrix_GR(); CDECL;
// Same as LineCodes_Get_Rmatrix but uses global result (GR) pointers
begin
    LineCodes_Get_Rmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineCodes_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;

begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(FNphases) - 1) + 1);
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

procedure LineCodes_Get_Xmatrix_GR(); CDECL;
// Same as LineCodes_Get_Xmatrix but uses global result (GR) pointers
begin
    LineCodes_Get_Xmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineCodes_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    Factor: Double;
    pLineCode: TLineCodeObj;

begin
    Value := PDoubleArray(ValuePtr);

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            Factor := TwoPi * BaseFrequency * 1.0e-9;
            k := (0);
            for i := 1 to FNPhases do
                for j := 1 to FNphases do
                begin
                    Yc.SetElement(i, j, Cmplx(0.0, Value[k] * Factor));
                    Inc(k);
                end;
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Ztemp: complex;

begin
    Value := PDoubleArray(ValuePtr);

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            k := (0);
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
//------------------------------------------------------------------------------
procedure LineCodes_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Ztemp: complex;

begin
    Value := PDoubleArray(ValuePtr);

    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        with pLineCode do
        begin
            k := (0);
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
//------------------------------------------------------------------------------
function LineCodes_Get_NormAmps(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.NormAmps;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_NormAmps(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        pLineCode.NormAmps := Value;
    end

end;
//------------------------------------------------------------------------------
function LineCodes_Get_EmergAmps(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        Result := pLineCode.EmergAmps;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_EmergAmps(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pLineCode := LineCodeClass.GetActiveObj;
        pLineCode.EmergAmps := Value;
    end

end;
//------------------------------------------------------------------------------
procedure LineCodes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, LineCodeClass.ElementList, False);
end;
//------------------------------------------------------------------------------
function LineCodes_Get_idx(): Integer; CDECL;
begin
    Result := LineCodeClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_idx(Value: Integer); CDECL;
begin
    if LineCodeClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg('Invalid LineCode index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
