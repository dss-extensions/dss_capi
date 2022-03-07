unit CAPI_LineGeometries;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    LineGeometry;

function LineGeometries_Get_Count(): Integer; CDECL;
function LineGeometries_Get_First(): Integer; CDECL;
function LineGeometries_Get_Next(): Integer; CDECL;
function LineGeometries_Get_Name(): PAnsiChar; CDECL;
procedure LineGeometries_Set_Name(const Value: PAnsiChar); CDECL;
function LineGeometries_Get_Nconds(): Integer; CDECL;
procedure LineGeometries_Set_Nconds(Value: Integer); CDECL;
function LineGeometries_Get_Phases(): Integer; CDECL;
procedure LineGeometries_Set_Phases(Value: Integer); CDECL;
procedure LineGeometries_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Cmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Rmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Xmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Zmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Zmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Units(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure LineGeometries_Get_Units_GR(); CDECL;
procedure LineGeometries_Set_Units(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
procedure LineGeometries_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LineGeometries_Get_Xcoords_GR(); CDECL;
procedure LineGeometries_Set_Xcoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LineGeometries_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LineGeometries_Get_Ycoords_GR(); CDECL;
procedure LineGeometries_Set_Ycoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LineGeometries_Get_Conductors(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure LineGeometries_Get_Conductors_GR(); CDECL;
function LineGeometries_Get_Reduce(): TAPIBoolean; CDECL;
procedure LineGeometries_Set_Reduce(Value: TAPIBoolean); CDECL;
function LineGeometries_Get_RhoEarth(): Double; CDECL;
procedure LineGeometries_Set_RhoEarth(Value: Double); CDECL;
function LineGeometries_Get_NormAmps(): Double; CDECL;
procedure LineGeometries_Set_NormAmps(Value: Double); CDECL;
function LineGeometries_Get_EmergAmps(): Double; CDECL;
procedure LineGeometries_Set_EmergAmps(Value: Double); CDECL;
procedure LineGeometries_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure LineGeometries_Get_AllNames_GR(); CDECL;

function LineGeometries_Get_idx(): Integer; CDECL;
procedure LineGeometries_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    UComplex, DSSUcomplex,
    Line,
    UcMatrix,
    DSSClass,
    DSSHelper;


//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TLineGeometryObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.LineGeometryClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['LineGeometry'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineGeometryClass.ElementCount;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineGeometryClass.First;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineGeometryClass.Next;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Name(): PAnsiChar; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, pLineGeometry.Name);
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if not DSSPrime.LineGeometryClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'LineGeometry "%s" not found in Active Circuit.', [Value], 51008);

     // Still same active object if not found
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Nconds(): Integer; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;

    Result := pLineGeometry.Nconds;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Nconds(Value: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if (Value < 1) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid number of conductors (%d). Please use a value within the valid range (>0).', [Value], 183);
        Exit;
    end;
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;

    pLineGeometry.DataChanged := TRUE;
    pLineGeometry.Nconds := Value;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Phases(): Integer; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;

    Result := pLineGeometry.NPhases;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Phases(Value: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if (Value < 1) then
    begin
        DoSimpleMsg(DSSPrime, _('Invalid number of phases sent via C-API. Please enter a value within range.'), 184);
    end;

    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;

    pLineGeometry.DataChanged := TRUE;
    pLineGeometry.NPhases := Value;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
var
    Result: PDoubleArray0;
    i, j, k: Integer;
    pLineGeometry: TLineGeometryObj;
    Factor: Double;
    mat: TcMatrix;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    mat := pLineGeometry.YCmatrix[Frequency, Length, Units];
    Factor := (TwoPi * Frequency * 1.0e-9);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(mat.Order));
    k := 0;
    for i := 1 to mat.Order do
        for j := 1 to mat.Order do
        begin
            Result[k] := mat.GetElement(i, j).im / Factor;
            Inc(k);
        end;
end;

procedure LineGeometries_Get_Cmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
// Same as LineGeometries_Get_Cmatrix but uses global result (GR) pointers
begin
    LineGeometries_Get_Cmatrix(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
var
    Result: PDoubleArray0;
    i, j, k: Integer;
    pLineGeometry: TLineGeometryObj;
    mat: Tcmatrix;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    mat := pLineGeometry.Zmatrix[Frequency, Length, Units];
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(mat.Order));
    k := 0;
    for i := 1 to mat.Order do
        for j := 1 to mat.Order do
        begin
            Result[k] := mat.GetElement(i, j).re;
            Inc(k);
        end;
end;

procedure LineGeometries_Get_Rmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
// Same as LineGeometries_Get_Rmatrix but uses global result (GR) pointers
begin
    LineGeometries_Get_Rmatrix(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
var
    Result: PDoubleArray0;
    i, j, k: Integer;
    pLineGeometry: TLineGeometryObj;
    mat: Tcmatrix;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    mat := pLineGeometry.Zmatrix[Frequency, Length, Units];
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, Sqr(mat.Order));
    k := 0;
    for i := 1 to mat.Order do
        for j := 1 to mat.Order do
        begin
            Result[k] := mat.GetElement(i, j).im;
            Inc(k);
        end;
end;

procedure LineGeometries_Get_Xmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
// Same as LineGeometries_Get_Xmatrix but uses global result (GR) pointers
begin
    LineGeometries_Get_Xmatrix(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Zmatrix(var ResultPtr: PDouble; ResultCount: PAPISize; Frequency, Length: Double; Units: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
    mat: Tcmatrix;
    order: Integer;
    data: PComplexArray;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    mat := pLineGeometry.Zmatrix[Frequency, Length, Units];
    data := mat.GetValuesArrayPtr(order);
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * Sqr(Order));
    Move(data[1], ResultPtr[0], ResultCount[0] * SizeOf(Double));
end;

procedure LineGeometries_Get_Zmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
// Same as LineGeometries_Get_Zmatrix but uses global result (GR) pointers
begin
    LineGeometries_Get_Zmatrix(DSSPrime.GR_DataPtr_PDouble, DSSPrime.GR_Counts_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
function LineGeometries_Get_Reduce(): TAPIBoolean; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    Result := pLineGeometry.FReduce;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Reduce(Value: TAPIBoolean); CDECL;
var
    pLineGeometry: TLineGeometryObj;

begin
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    pLineGeometry.DataChanged := TRUE;
    pLineGeometry.FReduce := Value;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_RhoEarth(): Double; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    Result := pLineGeometry.RhoEarth;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_RhoEarth(Value: Double); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    with pLineGeometry do
    begin
        RhoEarth := Value;
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_NormAmps(): Double; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    Result := pLineGeometry.NormAmps;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_NormAmps(Value: Double); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    pLineGeometry.NormAmps := Value;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_EmergAmps(): Double; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    Result := pLineGeometry.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_EmergAmps(Value: Double); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    pLineGeometry.EmergAmps := Value;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Units(ValuePtr: PInteger; ValueCount: TAPISize); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    with pLineGeometry do
    begin
        if Nconds <> ValueCount then
        begin
            DoSimpleMsg('The number of values provided (%d) does not match the number of conductors (%d).', [ValueCount, NConds], 183);
            Exit;
        end;
        Move(ValuePtr[0], FUnits[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Units(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray0;
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with pLineGeometry do
    begin
        DSS_RecreateArray_PInteger(Result, ResultPtr, ResultCount, Nconds);
        Move(FUnits[1], ResultPtr[0], Nconds * SizeOf(Integer));
    end;
end;

procedure LineGeometries_Get_Units_GR(); CDECL;
// Same as LineGeometries_Get_Units but uses global result (GR) pointers
begin
    LineGeometries_Get_Units(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Set_Ycoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    with pLineGeometry do
    begin
        if Nconds <> ValueCount then
        begin
            DoSimpleMsg('The number of values provided (%d) does not match the number of conductors (%d).', [ValueCount, NConds], 188);
            Exit;
        end;
        Move(ValuePtr[0], FY[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with pLineGeometry do
    begin
        DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, Nconds);
        Move(FY[1], ResultPtr[0], Nconds * SizeOf(Double));
    end;
end;

procedure LineGeometries_Get_Ycoords_GR(); CDECL;
// Same as LineGeometries_Get_Ycoords but uses global result (GR) pointers
begin
    LineGeometries_Get_Ycoords(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Set_Xcoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
        Exit;
    with pLineGeometry do
    begin
        if Nconds <> ValueCount then
        begin
            DoSimpleMsg('The number of values provided (%d) does not match the number of conductors (%d).', [ValueCount, NConds], 187);
            Exit;
        end;
        Move(ValuePtr[0], FX[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pLineGeometry: TLineGeometryObj;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    with pLineGeometry do
    begin
        DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, Nconds);
        Move(FX[1], ResultPtr[0], Nconds * SizeOf(Double));
    end;
end;

procedure LineGeometries_Get_Xcoords_GR(); CDECL;
// Same as LineGeometries_Get_Xcoords but uses global result (GR) pointers
begin
    LineGeometries_Get_Xcoords(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Conductors(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    pLineGeometry: TLineGeometryObj;
    i: Integer;
begin
    if not _activeObj(DSSPrime, pLineGeometry) then
    begin
        DefaultResult(ResultPtr, ResultCount);
    end;
    
    with pLineGeometry do
    begin
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, Nconds);
        for i := 1 to Nconds do
            Result[i - 1] := DSS_CopyStringAsPChar(ConductorName[i]);
    end;
end;

procedure LineGeometries_Get_Conductors_GR(); CDECL;
// Same as LineGeometries_Get_Conductors but uses global result (GR) pointers
begin
    LineGeometries_Get_Conductors(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.LineGeometryClass.ElementList, False);
end;

procedure LineGeometries_Get_AllNames_GR(); CDECL;
// Same as LineGeometries_Get_AllNames but uses global result (GR) pointers
begin
    LineGeometries_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function LineGeometries_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineGeometryClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_idx(Value: Integer); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.LineGeometryClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['LineGeometry', Value], 656565);
end;
//------------------------------------------------------------------------------
end.
