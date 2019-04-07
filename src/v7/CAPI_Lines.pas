unit CAPI_Lines;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Lines_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Lines_Get_AllNames_GR(); CDECL;
function Lines_Get_Bus1(): PAnsiChar; CDECL;
function Lines_Get_Bus2(): PAnsiChar; CDECL;
function Lines_Get_First(): Integer; CDECL;
function Lines_Get_Length(): Double; CDECL;
function Lines_Get_LineCode(): PAnsiChar; CDECL;
function Lines_Get_Name(): PAnsiChar; CDECL;
function Lines_Get_Next(): Integer; CDECL;
function Lines_Get_Phases(): Integer; CDECL;
function Lines_Get_R1(): Double; CDECL;
function Lines_Get_X1(): Double; CDECL;
function Lines_New(const Name: PAnsiChar): Integer; CDECL;
procedure Lines_Set_Bus1(const Value: PAnsiChar); CDECL;
procedure Lines_Set_Bus2(const Value: PAnsiChar); CDECL;
procedure Lines_Set_Length(Value: Double); CDECL;
procedure Lines_Set_LineCode(const Value: PAnsiChar); CDECL;
procedure Lines_Set_Name(const Value: PAnsiChar); CDECL;
procedure Lines_Set_Phases(Value: Integer); CDECL;
procedure Lines_Set_R1(Value: Double); CDECL;
procedure Lines_Set_X1(Value: Double); CDECL;
function Lines_Get_C0(): Double; CDECL;
function Lines_Get_C1(): Double; CDECL;
procedure Lines_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Lines_Get_Cmatrix_GR(); CDECL;
function Lines_Get_R0(): Double; CDECL;
procedure Lines_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Lines_Get_Rmatrix_GR(); CDECL;
function Lines_Get_X0(): Double; CDECL;
procedure Lines_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Lines_Get_Xmatrix_GR(); CDECL;
procedure Lines_Set_C0(Value: Double); CDECL;
procedure Lines_Set_C1(Value: Double); CDECL;
procedure Lines_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Lines_Set_R0(Value: Double); CDECL;
procedure Lines_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure Lines_Set_X0(Value: Double); CDECL;
procedure Lines_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function Lines_Get_EmergAmps(): Double; CDECL;
function Lines_Get_NormAmps(): Double; CDECL;
procedure Lines_Set_EmergAmps(Value: Double); CDECL;
procedure Lines_Set_NormAmps(Value: Double); CDECL;
function Lines_Get_Geometry(): PAnsiChar; CDECL;
procedure Lines_Set_Geometry(const Value: PAnsiChar); CDECL;
function Lines_Get_Rg(): Double; CDECL;
function Lines_Get_Rho(): Double; CDECL;
function Lines_Get_Xg(): Double; CDECL;
procedure Lines_Set_Rg(Value: Double); CDECL;
procedure Lines_Set_Rho(Value: Double); CDECL;
procedure Lines_Set_Xg(Value: Double); CDECL;
procedure Lines_Get_Yprim(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Lines_Get_Yprim_GR(); CDECL;
procedure Lines_Set_Yprim(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function Lines_Get_NumCust(): Integer; CDECL;
function Lines_Get_TotalCust(): Integer; CDECL;
function Lines_Get_Parent(): Integer; CDECL;
function Lines_Get_Count(): Integer; CDECL;
function Lines_Get_Spacing(): PAnsiChar; CDECL;
procedure Lines_Set_Spacing(const Value: PAnsiChar); CDECL;
function Lines_Get_Units(): Integer; CDECL;
procedure Lines_Set_Units(Value: Integer); CDECL;
function Lines_Get_SeasonRating(): Double; CDECL;

// API Extensions
function Lines_Get_idx(): Integer; CDECL;
procedure Lines_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    Line,
    DSSClassDefs,
    DSSGlobals,
    CktElement,
    uComplex,
    ExecHelper,
    Sysutils,
    ParserDel,
    Math,
    LineUnits,
    XYCurve;

//------------------------------------------------------------------------------
function IsLine(const CktElem: TDSSCktElement): Boolean;
begin
    Result := ((CktElem.DssObjtype and CLASSMASK) = LINE_ELEMENT);
    if not Result then
        DoSimpleMsg('Line Type Expected, but another found. Dss Class=' + CktElem.DSSClassName + CRLF +
            'Element name=' + CktElem.Name, 5007);
end;
//------------------------------------------------------------------------------
procedure Lines_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    LineElem: TLineObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    with ActiveCircuit do
    begin
        if Lines.ListSize <= 0 then Exit;
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, Lines.ListSize);
        k := 0;
        LineElem := Lines.First;
        while LineElem <> NIL do
        begin
            Result[k] := DSS_CopyStringAsPChar(LineElem.Name);
            Inc(k);
            LineElem := Lines.Next;
        end;
    end;
end;

procedure Lines_Get_AllNames_GR(); CDECL;
// Same as Lines_Get_AllNames but uses global result (GR) pointers
begin
    Lines_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Lines_Get_Bus1_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := ActiveCircuit.ActiveCktElement.GetBus(1);
end;

function Lines_Get_Bus1(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Bus1_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_Bus2_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := ActiveCircuit.ActiveCktElement.GetBus(2);
end;

function Lines_Get_Bus2(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Bus2_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_First(): Integer; CDECL;
var
    pLine: TLineObj;

begin
    Result := 0;  // signify no more
    if ActiveCircuit = NIL then 
        Exit;
    pLine := ActiveCircuit.Lines.First;
    if pLine = NIL then
        Exit;
    repeat
        if pLine.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pLine;
            Result := 1;
        end
        else
            pLine := ActiveCircuit.Lines.Next;
    until (Result = 1) or (pLine = NIL);
end;
//------------------------------------------------------------------------------
function Lines_Get_Length(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).Len;
end;
//------------------------------------------------------------------------------
function Lines_Get_LineCode_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).CondCode;
end;

function Lines_Get_LineCode(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_LineCode_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_Name_AnsiString(): Ansistring; inline;
var
    pLine: TDSSCktElement;
begin
    Result := '';  // signify no name
    if ActiveCircuit = NIL then Exit;
    pLine := ActiveCircuit.ActiveCktElement;
    if pLine = NIL then Exit;
    Result := pLine.Name;
end;

function Lines_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Lines_Get_Next(): Integer; CDECL;
var
    pLine: TLineObj;
begin
    Result := 0;  // signify no more
    if ActiveCircuit = NIL then Exit;
    pLine := ActiveCircuit.Lines.Next;
    if pLine = NIL then Exit;
    repeat
        if pLine.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pLine;
            Result := ActiveCircuit.Lines.ActiveIndex;
        end
        else
            pLine := ActiveCircuit.Lines.Next;
    until (Result > 0) or (pLine = NIL);
end;
//------------------------------------------------------------------------------
function Lines_Get_Phases(): Integer; CDECL;
begin
    Result := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := ActiveCircuit.ActiveCktElement.Nphases;
end;
//------------------------------------------------------------------------------
function Lines_Get_R1(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Result := R1 / UnitsConvert;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_X1(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Result := X1 / UnitsConvert;
    end;
end;
//------------------------------------------------------------------------------
function Lines_New(const Name: PAnsiChar): Integer; CDECL;
begin
    Result := AddObject('line', Name);    // Returns handle to object
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Bus1(const Value: PAnsiChar); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        SetBus(1, Value);
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Bus2(const Value: PAnsiChar); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        SetBus(2, Value);
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Length(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Len := Value;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_LineCode(const Value: PAnsiChar); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        FetchLineCode(Value);
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    if LineClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := LineClass.ElementList.Active;
        ActiveCircuit.Lines.Get(LineClass.Active);
    end
    else
    begin
        DoSimpleMsg('Line "' + Value + '" Not Found in Active Circuit.', 5008);
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Phases(Value: Integer); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Nphases := Value;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_R1(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        R1 := Value;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_X1(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        X1 := Value;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_C0(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;

    with TLineObj(ActiveCircuit.ActiveCktElement) do
        Result := C0 / UnitsConvert * 1.0e9;
end;
//------------------------------------------------------------------------------
function Lines_Get_C1(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
        Result := C1 / UnitsConvert * 1.0e9;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    Factor: Double;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    Result[0] := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Factor := TwoPi * BaseFrequency * 1.0e-9 * UnitsConvert;  // corrected 2.9.2018 RCD
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(Nphases));
        k := 0;
        for i := 1 to NPhases do
            for j := 1 to Nphases do
            begin
                Result[k] := Yc.GetElement(i, j).im / Factor;
                Inc(k);
            end;
    end;
end;

procedure Lines_Get_Cmatrix_GR(); CDECL;
// Same as Lines_Get_Cmatrix but uses global result (GR) pointers
begin
    Lines_Get_Cmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Lines_Get_R0(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Result := R0 / UnitsConvert;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    Result[0] := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
        k := 0;
        for i := 1 to NPhases do
            for j := 1 to Nphases do
            begin
                Result[k] := Z.GetElement(i, j).Re / UnitsConvert;
                Inc(k);
            end;
    end;
end;

procedure Lines_Get_Rmatrix_GR(); CDECL;
// Same as Lines_Get_Rmatrix but uses global result (GR) pointers
begin
    Lines_Get_Rmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function Lines_Get_X0(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Result := X0 / UnitsConvert;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    Result[0] := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(Nphases));
        k := 0;
        for i := 1 to NPhases do
            for j := 1 to Nphases do
            begin
                Result[k] := Z.GetElement(i, j).im / UnitsConvert;
                Inc(k);
            end;
    end;
end;

procedure Lines_Get_Xmatrix_GR(); CDECL;
// Same as Lines_Get_Xmatrix but uses global result (GR) pointers
begin
    Lines_Get_Xmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;
//------------------------------------------------------------------------------
procedure Lines_Set_C0(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        C0 := Value * 1.0e-9;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_C1(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        C1 := Value * 1.0e-9;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Cmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    Factor: Double;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Factor := TwoPi * BaseFrequency * 1.0e-9;
        k := (0);
        for i := 1 to NPhases do
            for j := 1 to Nphases do
            begin
                Yc.SetElement(i, j, Cmplx(0.0, Value[k] * Factor));
                Inc(k);
            end;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_R0(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        R0 := Value;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    Ztemp: complex;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        k := (0);
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
//------------------------------------------------------------------------------
procedure Lines_Set_X0(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        X0 := Value;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Xmatrix(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    Ztemp: complex;
begin
    Value := PDoubleArray(ValuePtr);
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        k := (0);
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
//------------------------------------------------------------------------------
function Lines_Get_EmergAmps(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).EmergAmps;
end;
//------------------------------------------------------------------------------
function Lines_Get_NormAmps(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_EmergAmps(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        EmergAmps := Value;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_NormAmps(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        NormAmps := Value;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_Geometry_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).GeometryCode;
end;

function Lines_Get_Geometry(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Geometry_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Geometry(const Value: PAnsiChar); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Parser.CmdString := 'geometry=' + Value;
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_Rg(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).Rg;
end;
//------------------------------------------------------------------------------
function Lines_Get_Rho(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).rho;
end;
//------------------------------------------------------------------------------
function Lines_Get_Xg(): Double; CDECL;
begin
    Result := 0.0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).Xg;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rg(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Parser.CmdString := Format('rg=%.7g', [Value]);
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rho(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Parser.CmdString := Format('rho=%.7g', [Value]);
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Xg(Value: Double); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Parser.CmdString := Format('xg=%.7g', [Value]);
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Yprim(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
{ Return the YPrim matrix for this element }

var
    Result: PDoubleArray;
    iV: Integer;
    i: Integer;
    NValues: Integer;
    cValues: pComplexArray;

begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);  // just return null array
        Result[0] := 0;
        Exit;
    end;
    with ActiveCircuit, TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        NValues := SQR(Yorder);
        cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
        if cValues = NIL then
        begin   // check for unassigned array
            Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);  // just return null array
            Exit;  // Get outta here
        end;
        
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
        iV := 0;
        Move(cValues[1], ResultPtr[0], 2 * NValues * SizeOf(Double));
    end
end;

procedure Lines_Get_Yprim_GR(); CDECL;
// Same as Lines_Get_Yprim but uses global result (GR) pointers
begin
    Lines_Get_Yprim(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Lines_Set_Yprim(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;

begin
    Value := PDoubleArray(ValuePtr);
    if ActiveCircuit <> NIL then
    begin
       {Do Nothing for now}
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_NumCust(): Integer; CDECL;
begin
    Result := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchNumCustomers;
end;
//------------------------------------------------------------------------------
function Lines_Get_TotalCust(): Integer; CDECL;
begin
    Result := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).BranchTotalCustomers;
end;
//------------------------------------------------------------------------------
function Lines_Get_Parent(): Integer; CDECL;
{ Sets the Active Line to the immediately upline Line obj, if any}
{ Returns line index  or 0 if it fails or no more lines}
var
    pLine: TLineObj;
begin
    Result := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
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
//------------------------------------------------------------------------------
function Lines_Get_Count(): Integer; CDECL;
begin
    if Assigned(Activecircuit) then
        Result := ActiveCircuit.Lines.ListSize;
end;
//------------------------------------------------------------------------------
function Lines_Get_Spacing_AnsiString(): Ansistring; inline;
begin
    Result := '';
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).SpacingCode;
end;

function Lines_Get_Spacing(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Lines_Get_Spacing_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Spacing(const Value: PAnsiChar); CDECL;
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    with TLineObj(ActiveCircuit.ActiveCktElement) do
    begin
        Parser.CmdString := 'spacing=' + Value;
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_Units(): Integer; CDECL;
begin
    Result := 0;
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
    Result := TLineObj(ActiveCircuit.ActiveCktElement).LengthUnits;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Units(Value: Integer); CDECL;
{
 This code assumes the present value of line units is NONE.
 The Set functions in this interface all set values in this length unit.
}
begin
    if (ActiveCircuit = NIL) or (not IsLine(ActiveCircuit.ActiveCktElement)) then
        Exit;
        
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
//------------------------------------------------------------------------------
function Lines_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Lines.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_idx(Value: Integer); CDECL;
var
    pLine: TLineObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pLine := ActiveCircuit.Lines.Get(Value);
    if pLine = NIL then
    begin
        DoSimpleMsg('Invalid Line index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pLine;
end;
//------------------------------------------------------------------------------
function Lines_Get_SeasonRating(): Double; CDECL;
var
    RatingIdx: Integer;
    RSignal: TXYCurveObj;
begin
    Result := 0;
    DoSimpleMsg('Not implemented.', 999999);
//    if not IsLine(ActiveCircuit.ActiveCktElement) then 
//        Exit;
//        
//    if not SeasonalRating then 
//        Exit;
//        
//    if SeasonSignal <> '' then
//    begin
//        RSignal := XYCurveClass.Find(SeasonSignal);
//        
//        if RSignal <> NIL then
//            RatingIdx := trunc(RSignal.GetYValue(ActiveCircuit.Solution.DynaVars.intHour)) + 1;
//        
//        // Just in case
//        if RatingIdx > TLineObj(ActiveCircuit.ActiveCktElement).NRatings then
//            Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps
//        else
//            Result := TLineObj(ActiveCircuit.ActiveCktElement).ratings^[RatingIdx];
//    end
//    else
//        Result := TLineObj(ActiveCircuit.ActiveCktElement).NormAmps;
end;
//------------------------------------------------------------------------------
end.
