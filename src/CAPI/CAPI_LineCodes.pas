unit CAPI_LineCodes;

interface

uses
    CAPI_Utils,
    LineCode;

function LineCodes_Get_Count(): Integer; CDECL;
function LineCodes_Get_First(): Integer; CDECL;
function LineCodes_Get_Next(): Integer; CDECL;
function LineCodes_Get_Name(): PAnsiChar; CDECL;
procedure LineCodes_Set_Name(const Value: PAnsiChar); CDECL;
function LineCodes_Get_IsZ1Z0(): TAPIBoolean; CDECL;
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
procedure LineCodes_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LineCodes_Get_Cmatrix_GR(); CDECL;
procedure LineCodes_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LineCodes_Get_Rmatrix_GR(); CDECL;
procedure LineCodes_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LineCodes_Get_Xmatrix_GR(); CDECL;
procedure LineCodes_Set_Cmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LineCodes_Set_Rmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LineCodes_Set_Xmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function LineCodes_Get_NormAmps(): Double; CDECL;
procedure LineCodes_Set_NormAmps(Value: Double); CDECL;
function LineCodes_Get_EmergAmps(): Double; CDECL;
procedure LineCodes_Set_EmergAmps(Value: Double); CDECL;
procedure LineCodes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure LineCodes_Get_AllNames_GR(); CDECL;

function LineCodes_Get_idx(): Integer; CDECL;
procedure LineCodes_Set_idx(Value: Integer); CDECL;

implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    ParserDel,
    Ucomplex,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSSPrime: TDSSContext; out obj: TLineCodeObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    
    obj := DSSPrime.LineCodeClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSSPrime, 'No active LineCode object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineCodeClass.ElementCount;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineCodeClass.First;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineCodeClass.Next;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Name(): PAnsiChar; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;
    
    Result := DSS_GetAsPAnsiChar(pLineCode.Name);
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    if not DSSPrime.LineCodeClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'LineCode "' + Value + '" Not Found in Active Circuit.', 51008);

    // Still same active object if not found
end;
//------------------------------------------------------------------------------
function LineCodes_Get_IsZ1Z0(): TAPIBoolean; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := TRUE;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;
    
    Result := pLineCode.SymComponentsModel;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Units(): Integer; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;
    
    Result := pLineCode.Units;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Units(Value: Integer); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    with pLineCode do
    begin
        if Value < dssLineUnitsMaxnum then
        begin
            DSSPrime.Parser.CmdString := Format('units=%s', [LineUnitsStr(Value)]);
            Edit;
        end
        else
            DoSimpleMsg('Invalid line units integer sent via COM interface.  Please enter a value within range.', 183);
    end;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_Phases(): Integer; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;
        
    Result := pLineCode.FNPhases;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Phases(Value: Integer); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    pLineCode.NumPhases := Value;   // use property value to force reallocations
end;
//------------------------------------------------------------------------------
function LineCodes_Get_R1(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Result := pLineCode.R1;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_R1(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    DSSPrime.Parser.CmdString := Format('R1=%g', [Value]);
    pLineCode.Edit;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_X1(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Result := pLineCode.X1;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_X1(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    DSSPrime.Parser.CmdString := Format('X1=%g', [Value]);
    pLineCode.Edit;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_R0(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;
    
    Result := pLineCode.R0;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_X0(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Result := pLineCode.X0;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_R0(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    DSSPrime.Parser.CmdString := Format('R0=%g', [Value]);
    pLineCode.Edit;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_X0(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    DSSPrime.Parser.CmdString := Format('X0=%g', [Value]);
    pLineCode.Edit;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_C0(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Result := pLineCode.C0;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_C1(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Result := pLineCode.C1;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_C0(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    DSSPrime.Parser.CmdString := Format('C0=%g', [Value]);
    pLineCode.Edit;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_C1(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    DSSPrime.Parser.CmdString := Format('C1=%g', [Value]);
    pLineCode.Edit;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Factor: Double;
begin
    if not _activeObj(DSSPrime, pLineCode) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with pLineCode do
    begin
        Factor := (TwoPi * BaseFrequency * 1.0e-9);
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(FNphases));
        k := 0;
        for i := 1 to FNPhases do
            for j := 1 to FNphases do
            begin
                Result[k] := YC.GetElement(i, j).im / Factor;
                Inc(k);
            end;
    end;
end;

procedure LineCodes_Get_Cmatrix_GR(); CDECL;
// Same as LineCodes_Get_Cmatrix but uses global result (GR) pointers
begin
    LineCodes_Get_Cmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineCodes_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;

begin
    if not _activeObj(DSSPrime, pLineCode) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with pLineCode do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(FNphases));
        k := 0;
        for i := 1 to FNPhases do
            for j := 1 to FNphases do
            begin
                Result[k] := Z.GetElement(i, j).re;
                Inc(k);
            end;
    end;
end;

procedure LineCodes_Get_Rmatrix_GR(); CDECL;
// Same as LineCodes_Get_Rmatrix but uses global result (GR) pointers
begin
    LineCodes_Get_Rmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineCodes_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;

begin
    if not _activeObj(DSSPrime, pLineCode) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with pLineCode do
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(FNphases));
        k := 0;
        for i := 1 to FNPhases do
            for j := 1 to FNphases do
            begin
                Result[k] := Z.GetElement(i, j).im;
                Inc(k);
            end;
    end;
end;

procedure LineCodes_Get_Xmatrix_GR(); CDECL;
// Same as LineCodes_Get_Xmatrix but uses global result (GR) pointers
begin
    LineCodes_Get_Xmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineCodes_Set_Cmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    Factor: Double;
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Value := PDoubleArray(ValuePtr);

    with pLineCode do
    begin
        if (FNPhases * FNPhases) <> ValueCount then
        begin
            DoSimpleMsg(Format(
                'The number of values provided (%d) does not match the expected (%d).', 
                [ValueCount, FNPhases * FNPhases]
            ), 183);
            Exit;
        end;
    
        Factor := TwoPi * BaseFrequency * 1.0e-9;
        k := 0;
        for i := 1 to FNPhases do
            for j := 1 to FNphases do
            begin
                Yc.SetElement(i, j, Cmplx(0.0, Value[k] * Factor));
                Inc(k);
            end;
    end;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Rmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Ztemp: complex;

begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Value := PDoubleArray(ValuePtr);

    with pLineCode do
    begin
        if (FNPhases * FNPhases) <> ValueCount then
        begin
            DoSimpleMsg(Format(
                'The number of values provided (%d) does not match the expected (%d).', 
                [ValueCount, FNPhases * FNPhases]
            ), 183);
            Exit;
        end;

        k := 0;
        for i := 1 to FNPhases do
            for j := 1 to FNphases do
            begin
                ZTemp := Z.GetElement(i, j);
                Z.SetElement(i, j, Cmplx(Value[k], ZTemp.im));
                Inc(k);
            end;
    end;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_Xmatrix(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    Value: PDoubleArray;
    i, j, k: Integer;
    pLineCode: TLineCodeObj;
    Ztemp: complex;

begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Value := PDoubleArray(ValuePtr);

    with pLineCode do
    begin
        if (FNPhases * FNPhases) <> ValueCount then
        begin
            DoSimpleMsg(Format(
                'The number of values provided (%d) does not match the expected (%d).', 
                [ValueCount, FNPhases * FNPhases]
            ), 183);
            Exit;
        end;
    
        k := 0;
        for i := 1 to FNPhases do
            for j := 1 to FNphases do
            begin
                ZTemp := Z.GetElement(i, j);
                Z.SetElement(i, j, Cmplx(ZTemp.re, Value[k]));
                Inc(k);
            end;
    end;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_NormAmps(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Result := pLineCode.NormAmps;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_NormAmps(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    pLineCode.NormAmps := Value;
end;
//------------------------------------------------------------------------------
function LineCodes_Get_EmergAmps(): Double; CDECL;
var
    pLineCode: TLineCodeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;

    Result := pLineCode.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_EmergAmps(Value: Double); CDECL;
var
    pLineCode: TLineCodeObj;
begin
    if not _activeObj(DSSPrime, pLineCode) then
        Exit;
        
    pLineCode.EmergAmps := Value;
end;
//------------------------------------------------------------------------------
procedure LineCodes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.LineCodeClass.ElementList, False);
end;

procedure LineCodes_Get_AllNames_GR(); CDECL;
// Same as LineCodes_Get_AllNames but uses global result (GR) pointers
begin
    LineCodes_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function LineCodes_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineCodeClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LineCodes_Set_idx(Value: Integer); CDECL;
begin
    if DSSPrime.LineCodeClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg(DSSPrime, 'Invalid LineCode index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
