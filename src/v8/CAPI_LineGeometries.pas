unit CAPI_LineGeometries;

{$inline on}

interface

uses
    CAPI_Utils,
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
procedure LineGeometries_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Cmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Rmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Xmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Zmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Zmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
procedure LineGeometries_Get_Units(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
procedure LineGeometries_Get_Units_GR(); CDECL;
procedure LineGeometries_Set_Units(ValuePtr: PInteger; ValueCount: Integer); CDECL;
procedure LineGeometries_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LineGeometries_Get_Xcoords_GR(); CDECL;
procedure LineGeometries_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LineGeometries_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LineGeometries_Get_Ycoords_GR(); CDECL;
procedure LineGeometries_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LineGeometries_Get_Conductors(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure LineGeometries_Get_Conductors_GR(); CDECL;
function LineGeometries_Get_Reduce(): Wordbool; CDECL;
procedure LineGeometries_Set_Reduce(Value: Wordbool); CDECL;
function LineGeometries_Get_RhoEarth(): Double; CDECL;
procedure LineGeometries_Set_RhoEarth(Value: Double); CDECL;
function LineGeometries_Get_NormAmps(): Double; CDECL;
procedure LineGeometries_Set_NormAmps(Value: Double); CDECL;
function LineGeometries_Get_EmergAmps(): Double; CDECL;
procedure LineGeometries_Set_EmergAmps(Value: Double); CDECL;
procedure LineGeometries_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure LineGeometries_Get_AllNames_GR(); CDECL;

function LineGeometries_Get_idx(): Integer; CDECL;
procedure LineGeometries_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    Ucomplex,
    Line,
    UcMatrix;

function LineGeometries_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    Result := LineGeometryClass[ActiveActor].ElementCount;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    Result := LineGeometryClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    Result := LineGeometryClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Name_AnsiString(): Ansistring; inline;
var
    pLineGeometry: TLineGeometryObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    if pLineGeometry <> NIL then
    begin
        Result := pLineGeometry.Name;
    end;
end;

function LineGeometries_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(LineGeometries_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    if not LineGeometryClass[ActiveActor].SetActive(Value) then
        DoSimpleMsg('LineGeometry "' + Value + '" Not Found in Active Circuit.', 51008);

     // Still same active object if not found
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Nconds(): Integer; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    Result := pLineGeometry.Nconds;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Nconds(Value: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    if (Value < 1) then
    begin
        DoSimpleMsg('Invalid number of conductors sent via C-API. Please enter a value within range.', 183);
        Exit;
    end;
    pLineGeometry.DataChanged := TRUE;
    pLineGeometry.Nconds := Value;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_Phases(): Integer; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    Result := pLineGeometry.NPhases;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Phases(Value: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    if (Value < 1) then
    begin
        DoSimpleMsg('Invalid number of phases sent via C-API. Please enter a value within range.', 184);
    end;
    
    with pLineGeometry do
    begin
        DataChanged := TRUE;
        NPhases := Value;
    end;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Cmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineGeometry: TLineGeometryObj;
    Factor: Double;
    mat: TcMatrix;
begin
    if ActiveCircuit[ActiveActor] = NIL then 
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
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
    LineGeometries_Get_Cmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Rmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineGeometry: TLineGeometryObj;
    mat: Tcmatrix;
begin
    if ActiveCircuit[ActiveActor] = NIL then 
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
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
    LineGeometries_Get_Rmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Xmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
var
    Result: PDoubleArray;
    i, j, k: Integer;
    pLineGeometry: TLineGeometryObj;
    mat: Tcmatrix;
begin
    if ActiveCircuit[ActiveActor] = NIL then 
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
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
    LineGeometries_Get_Xmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Zmatrix(var ResultPtr: PDouble; ResultCount: PInteger; Frequency, Length: Double; Units: Integer); CDECL;
var
    Result: PDoubleArray;
    pLineGeometry: TLineGeometryObj;
    mat: Tcmatrix;
    order: Integer;
    data: PComplexArray;
begin
    if ActiveCircuit[ActiveActor] = NIL then 
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    mat := pLineGeometry.Zmatrix[Frequency, Length, Units];
    data := mat.GetValuesArrayPtr(order);
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 2 * Sqr(Order));
    Move(data[1], ResultPtr[0], ResultCount[0] * SizeOf(Double));
end;

procedure LineGeometries_Get_Zmatrix_GR(Frequency, Length: Double; Units: Integer); CDECL;
// Same as LineGeometries_Get_Zmatrix but uses global result (GR) pointers
begin
    LineGeometries_Get_Zmatrix(GR_DataPtr_PDouble, GR_CountPtr_PDouble, Frequency, Length, Units)
end;

//------------------------------------------------------------------------------
function LineGeometries_Get_Reduce(): Wordbool; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := FALSE;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    Result := pLineGeometry.FReduce;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Reduce(Value: Wordbool); CDECL;
var
    pLineGeometry: TLineGeometryObj;

begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        DataChanged := TRUE;
        FReduce := Value;
    end;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_RhoEarth(): Double; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    Result := pLineGeometry.RhoEarth;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_RhoEarth(Value: Double); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
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
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    Result := pLineGeometry.NormAmps;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_NormAmps(Value: Double); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    pLineGeometry.NormAmps := Value;
end;
//------------------------------------------------------------------------------
function LineGeometries_Get_EmergAmps(): Double; CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    Result := pLineGeometry.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_EmergAmps(Value: Double); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    pLineGeometry.EmergAmps := Value;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_Units(ValuePtr: PInteger; ValueCount: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        if Nconds <> ValueCount then
        begin
            DoSimpleMsg('Invalid number of items sent via C-API. Please enter a value within range.', 189);
            Exit;
        end;
        Move(ValuePtr[0], FUnits[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Units(var ResultPtr: PInteger; ResultCount: PInteger); CDECL;
var
    Result: PIntegerArray;
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then 
    begin
        Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        DSS_RecreateArray_PInteger(Result, ResultPtr, ResultCount, Nconds);
        Move(FUnits[1], ResultPtr[0], Nconds * SizeOf(Integer));
    end;
end;

procedure LineGeometries_Get_Units_GR(); CDECL;
// Same as LineGeometries_Get_Units but uses global result (GR) pointers
begin
    LineGeometries_Get_Units(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        if Nconds <> ValueCount then
        begin
            DoSimpleMsg('Invalid number of items sent via C-API. Please enter a value within range.', 188);
            Exit;
        end;
        Move(ValuePtr[0], FY[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then 
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, Nconds);
        Move(FY[1], ResultPtr[0], Nconds * SizeOf(Double));
    end;
end;

procedure LineGeometries_Get_Ycoords_GR(); CDECL;
// Same as LineGeometries_Get_Ycoords but uses global result (GR) pointers
begin
    LineGeometries_Get_Ycoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        if Nconds <> ValueCount then
        begin
            DoSimpleMsg('Invalid number of items sent via C-API. Please enter a value within range.', 187);
            Exit;
        end;
        Move(ValuePtr[0], FX[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pLineGeometry: TLineGeometryObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then 
    begin
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Result[0] := 0;
        Exit;
    end;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, Nconds);
        Move(FX[1], ResultPtr[0], Nconds * SizeOf(Double));
    end;
end;

procedure LineGeometries_Get_Xcoords_GR(); CDECL;
// Same as LineGeometries_Get_Xcoords but uses global result (GR) pointers
begin
    LineGeometries_Get_Xcoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_Conductors(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    pLineGeometry: TLineGeometryObj;
    i: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    pLineGeometry := LineGeometryClass[ActiveActor].GetActiveObj;
    with pLineGeometry do
    begin
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Nconds - 1) + 1);
        for i := 1 to Nconds do
            Result[i - 1] := DSS_CopyStringAsPChar(ConductorName[i]);
    end;
end;

procedure LineGeometries_Get_Conductors_GR(); CDECL;
// Same as LineGeometries_Get_Conductors but uses global result (GR) pointers
begin
    LineGeometries_Get_Conductors(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure LineGeometries_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    LineGeometryElem: TLineGeometryObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    if LineGeometryClass[ActiveActor].ElementList.ListSize <= 0 then Exit;
    DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, LineGeometryClass[ActiveActor].ElementList.ListSize);
    k := 0;
    LineGeometryElem := LineGeometryClass[ActiveActor].ElementList.First;
    while LineGeometryElem <> NIL do
    begin
        Result[k] := DSS_CopyStringAsPChar(LineGeometryElem.Name);
        Inc(k);
        LineGeometryElem := LineGeometryClass[ActiveActor].ElementList.Next;
    end;
end;

procedure LineGeometries_Get_AllNames_GR(); CDECL;
// Same as LineGeometries_Get_AllNames but uses global result (GR) pointers
begin
    LineGeometries_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function LineGeometries_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := LineGeometryClass[ActiveActor].ElementList.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure LineGeometries_Set_idx(Value: Integer); CDECL;
begin
    if ActiveCircuit[ActiveActor] = NIL then Exit;
    if LineGeometryClass[ActiveActor].ElementList.Get(Value) = NIL then
        DoSimpleMsg('Invalid LineGeometry index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
