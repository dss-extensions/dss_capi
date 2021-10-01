unit CAPI_Lines;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Lines_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
procedure Lines_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Lines_Get_Cmatrix_GR(); CDECL;
function Lines_Get_R0(): Double; CDECL;
procedure Lines_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Lines_Get_Rmatrix_GR(); CDECL;
function Lines_Get_X0(): Double; CDECL;
procedure Lines_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Lines_Get_Xmatrix_GR(); CDECL;
procedure Lines_Set_C0(Value: Double); CDECL;
procedure Lines_Set_C1(Value: Double); CDECL;
procedure Lines_Set_Cmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Lines_Set_R0(Value: Double); CDECL;
procedure Lines_Set_Rmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure Lines_Set_X0(Value: Double); CDECL;
procedure Lines_Set_Xmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
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
procedure Lines_Get_Yprim(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Lines_Get_Yprim_GR(); CDECL;
procedure Lines_Set_Yprim(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
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
function Lines_Get_IsSwitch(): TAPIBoolean; CDECL;
procedure Lines_Set_IsSwitch(Value: TAPIBoolean); CDECL;

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
    XYCurve,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TLineObj): Boolean; inline;
var
    CktElem: TDSSCktElement;
begin
    Result := False;
    obj := NIL;
    
    if InvalidCircuit(DSS) then
        Exit;

    //TODO: use the active line instead?
    
    CktElem := DSS.ActiveCircuit.ActiveCktElement;
    if CktElem = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active Line object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    if CktElem is TLineObj then
        obj := CktElem as TLineObj;
        
    if obj = NIL {((CktElem.DssObjtype and CLASSMASK) <> LINE_ELEMENT)} then
    begin
        DoSimpleMsg(DSS, 'Line Type Expected, but another found. DSS Class=' + CktElem.DSSClassName + CRLF +
            'Element name=' + CktElem.Name, 5007);
        Exit;
    end;

    Result := True;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Lines, False);
end;

procedure Lines_Get_AllNames_GR(); CDECL;
// Same as Lines_Get_AllNames but uses global result (GR) pointers
begin
    Lines_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Lines_Get_Bus1(): PAnsiChar; CDECL;
var
    elem: TLineObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.GetBus(1));
end;
//------------------------------------------------------------------------------
function Lines_Get_Bus2(): PAnsiChar; CDECL;
var
    elem: TLineObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.GetBus(2));
end;
//------------------------------------------------------------------------------
function Lines_Get_First(): Integer; CDECL;
begin
    Result := 0;  // signify no more
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Lines);
end;
//------------------------------------------------------------------------------
function Lines_Get_Next(): Integer; CDECL;
begin
    Result := 0;  // signify no more
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Lines);
end;
//------------------------------------------------------------------------------
function Lines_Get_Length(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Len;
end;
//------------------------------------------------------------------------------
function Lines_Get_LineCode(): PAnsiChar; CDECL;
var
    elem: TLineObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.CondCode);
end;
//------------------------------------------------------------------------------
function Lines_Get_Name(): PAnsiChar; CDECL;
var
    elem: TLineObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
function Lines_Get_Phases(): Integer; CDECL;
var
    elem: TLineObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Nphases;
end;
//------------------------------------------------------------------------------
function Lines_Get_R1(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.R1 / elem.UnitsConvert;
end;
//------------------------------------------------------------------------------
function Lines_Get_X1(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.X1 / elem.UnitsConvert;
end;
//------------------------------------------------------------------------------
function Lines_New(const Name: PAnsiChar): Integer; CDECL;
begin
    Result := DSSPrime.DSSExecutive.AddObject('line', Name);    // Returns handle to object
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Bus1(const Value: PAnsiChar); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.SetBus(1, Value);
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Bus2(const Value: PAnsiChar); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.SetBus(2, Value);
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Length(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Len := Value;
    elem.YprimInvalid := TRUE;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_LineCode(const Value: PAnsiChar); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FetchLineCode(Value);
    elem.YprimInvalid := TRUE;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.LineClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.LineClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Lines.Get(DSSPrime.LineClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Line "' + Value + '" Not Found in Active Circuit.', 5008);
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Phases(Value: Integer); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Nphases := Value;
    elem.YprimInvalid := TRUE;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_R1(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.R1 := Value;
    elem.SymComponentsChanged := TRUE;
    elem.YprimInvalid := TRUE;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_X1(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.X1 := Value;
    elem.SymComponentsChanged := TRUE;
    elem.YprimInvalid := TRUE;
end;
//------------------------------------------------------------------------------
function Lines_Get_C0(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.C0 / elem.UnitsConvert * 1.0e9;
end;
//------------------------------------------------------------------------------
function Lines_Get_C1(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.C1 / elem.UnitsConvert * 1.0e9;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    i, j, k: Integer;
    Factor: Double;
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with elem do
    begin
        Factor := TwoPi * BaseFrequency * 1.0e-9 * UnitsConvert;  // corrected 2.9.2018 RCD
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(Nphases));
        k := 0;
        for i := 1 to NPhases do
            for j := 1 to Nphases do
            begin
                if GeometrySpecified Or SpacingSpecified then  
                    Result[k] := Yc.GetElement(i, j).im / Factor / Len
                else 
                    Result[k] := Yc.GetElement(i, j).im / Factor;

                Inc(k);
            end;
    end;
end;

procedure Lines_Get_Cmatrix_GR(); CDECL;
// Same as Lines_Get_Cmatrix but uses global result (GR) pointers
begin
    Lines_Get_Cmatrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Lines_Get_R0(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.R0 / elem.UnitsConvert;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    i, j, k: Integer;
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with elem do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (Sqr(Nphases) - 1) + 1);
        k := 0;
        for i := 1 to NPhases do
            for j := 1 to Nphases do
            begin
                if GeometrySpecified Or SpacingSpecified then
                    Result[k] := Z.GetElement(i, j).Re / Len
                else 
                    Result[k] := Z.GetElement(i, j).Re / UnitsConvert;

                Inc(k);
            end;
    end;
end;

procedure Lines_Get_Rmatrix_GR(); CDECL;
// Same as Lines_Get_Rmatrix but uses global result (GR) pointers
begin
    Lines_Get_Rmatrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function Lines_Get_X0(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.X0 / elem.UnitsConvert;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    i, j, k: Integer;
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with elem do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(Nphases));
        k := 0;
        for i := 1 to NPhases do
            for j := 1 to Nphases do
            begin
                if GeometrySpecified Or SpacingSpecified then
                    Result[k] := Z.GetElement(i, j).im / Len
                else
                    Result[k] := Z.GetElement(i, j).im / UnitsConvert;

                Inc(k);
            end;
    end;
end;

procedure Lines_Get_Xmatrix_GR(); CDECL;
// Same as Lines_Get_Xmatrix but uses global result (GR) pointers
begin
    Lines_Get_Xmatrix(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
procedure Lines_Set_C0(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        C0 := Value * 1.0e-9;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_C1(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        C1 := Value * 1.0e-9;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Cmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    i, j, k: Integer;
    Factor: Double;
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Value := PDoubleArray0(ValuePtr);
    with elem do
    begin
        if (NPhases * NPhases) <> ValueCount then
        begin
            DoSimpleMsg(Format(
                'The number of values provided (%d) does not match the expected (%d).', 
                [ValueCount, NPhases * NPhases]
            ), 183);
            Exit;
        end;
    
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
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        R0 := Value;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    i, j, k: Integer;
    Ztemp: complex;
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Value := PDoubleArray0(ValuePtr);
    with elem do
    begin
        if (NPhases * NPhases) <> ValueCount then
        begin
            DoSimpleMsg(Format(
                'The number of values provided (%d) does not match the expected (%d).', 
                [ValueCount, NPhases * NPhases]
            ), 183);
            Exit;
        end;
    
        k := 0;
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
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        X0 := Value;
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Xmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray0;
    i, j, k: Integer;
    Ztemp: complex;
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Value := PDoubleArray0(ValuePtr);
    
    with elem do
    begin
        if (NPhases * NPhases) <> ValueCount then
        begin
            DoSimpleMsg(Format('The number of values provided (%d) does not match the expected (%d).', 
                [ValueCount, NPhases * NPhases]
            ), 183);
            Exit;
        end;

        k := 0;
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
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.EmergAmps;
end;
//------------------------------------------------------------------------------
function Lines_Get_NormAmps(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NormAmps;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_EmergAmps(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.EmergAmps := Value;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_NormAmps(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.NormAmps := Value;
end;
//------------------------------------------------------------------------------
function Lines_Get_Geometry(): PAnsiChar; CDECL;
var
    elem: TLineObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.GeometryCode);
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Geometry(const Value: PAnsiChar); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        DSSPrime.Parser.CmdString := 'geometry=' + Value;
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_Rg(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Rg;
end;
//------------------------------------------------------------------------------
function Lines_Get_Rho(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.rho;
end;
//------------------------------------------------------------------------------
function Lines_Get_Xg(): Double; CDECL;
var
    elem: TLineObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Xg;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rg(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        DSSPrime.Parser.CmdString := Format('rg=%.7g', [Value]);
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Rho(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        DSSPrime.Parser.CmdString := Format('rho=%.7g', [Value]);
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Xg(Value: Double); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        DSSPrime.Parser.CmdString := Format('xg=%.7g', [Value]);
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure Lines_Get_Yprim(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
{ Return the YPrim matrix for this element }
var
    NValues: Integer;
    cValues: pComplexArray;
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with elem do
    begin
        NValues := SQR(Yorder);
        cValues := GetYprimValues(ALL_YPRIM);  // Get pointer to complex array of values
        if cValues = NIL then
        begin   // check for unassigned array
            DefaultResult(ResultPtr, ResultCount);
            Exit;  // Get outta here
        end;
        
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * NValues);
        Move(cValues[1], ResultPtr[0], 2 * NValues * SizeOf(Double));
    end
end;

procedure Lines_Get_Yprim_GR(); CDECL;
// Same as Lines_Get_Yprim but uses global result (GR) pointers
begin
    Lines_Get_Yprim(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure Lines_Set_Yprim(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
        
    {Do Nothing for now}
    
    DoSimpleMsg(DSSPrime, 'Setting Yprim is currently not allowed.', 1833);
end;
//------------------------------------------------------------------------------
function Lines_Get_NumCust(): Integer; CDECL;
var
    elem: TLineObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.BranchNumCustomers;
end;
//------------------------------------------------------------------------------
function Lines_Get_TotalCust(): Integer; CDECL;
var
    elem: TLineObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.BranchTotalCustomers;
end;
//------------------------------------------------------------------------------
function Lines_Get_Parent(): Integer; CDECL;
{ Sets the Active Line to the immediately upline Line obj, if any}
{ Returns line index  or 0 if it fails or no more lines}
var
    pLine: TLineObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLine) then
        Exit;

    if pLine.ParentPDelement = NIL then
        Exit;
        
    if (pLine.ParentPDelement.Enabled and ((pLine.ParentPDelement.DssObjtype and CLASSMASK) = LINE_ELEMENT)) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := pLine.ParentPDElement;
        Result := DSSPrime.ActiveCircuit.Lines.ActiveIndex;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Lines.Count;
end;
//------------------------------------------------------------------------------
function Lines_Get_Spacing(): PAnsiChar; CDECL;
var
    elem: TLineObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.SpacingCode);
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Spacing(const Value: PAnsiChar); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        DSSPrime.Parser.CmdString := 'spacing=' + Value;
        Edit;
        YprimInvalid := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_Units(): Integer; CDECL;
var
    elem: TLineObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.LengthUnits;
end;
//------------------------------------------------------------------------------
procedure Lines_Set_Units(Value: Integer); CDECL;
{
 This code assumes the present value of line units is NONE.
 The Set functions in this interface all set values in this length unit.
}
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    with elem do
    begin
        if (Value >= dssLineUnitsNone) and (Value < dssLineUnitsMaxnum) then
        begin
            DSSPrime.Parser.CmdString := Format('units=%s', [LineUnitsStr(Value)]);
            Edit;
            YprimInvalid := TRUE;
        end
        else
            DoSimpleMsg('Invalid line units code. Please enter a value within range.', 183);
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Lines.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Lines_Set_idx(Value: Integer); CDECL;
var
    pLine: TLineObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pLine := DSSPrime.ActiveCircuit.Lines.Get(Value);
    if pLine = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Line index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pLine;
end;
//------------------------------------------------------------------------------
function Lines_Get_SeasonRating(): Double; CDECL;
var
    RatingIdx: Integer;
    RSignal: TXYCurveObj;
    elem: TLineObj;
begin
    Result := 0;
    RatingIdx := -1;

    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (not DSSPrime.SeasonalRating) or (DSSPrime.SeasonSignal = '') then 
    begin
        Result := elem.NormAmps;
        Exit;
    end;
    
    RSignal := DSSPrime.XYCurveClass.Find(DSSPrime.SeasonSignal);
    if RSignal <> NIL then
        RatingIdx := trunc(RSignal.GetYValue(DSSPrime.ActiveCircuit.Solution.DynaVars.intHour));
    
    // Just in case
    if (RatingIdx >= elem.NumAmpRatings) or (RatingIdx < 0) then
        Result := elem.NormAmps
    else
        Result := elem.AmpRatings[RatingIdx];
end;
//------------------------------------------------------------------------------
procedure Lines_Set_IsSwitch(Value: TAPIBoolean); CDECL;
var
    elem: TLineObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
        
    with elem do
    begin
        IsSwitch := Value;
        if not Value then Exit;
        
        // Side effects from Line.pas
        SymComponentsChanged := TRUE;
        YprimInvalid := TRUE;
        GeometrySpecified := FALSE;
        SpacingSpecified := FALSE;
        r1 := 1.0;
        x1 := 1.0;
        r0 := 1.0;
        x0 := 1.0;
        c1 := 1.1 * 1.0e-9;
        c0 := 1.0 * 1.0e-9;
        len := 0.001;
        ResetLengthUnits;
    end;
end;
//------------------------------------------------------------------------------
function Lines_Get_IsSwitch(): TAPIBoolean; CDECL;
var
    elem: TLineObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.IsSwitch;
end;
//------------------------------------------------------------------------------
end.
