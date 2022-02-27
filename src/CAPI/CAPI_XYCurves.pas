unit CAPI_XYCurves;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    XYCurve,
    DSSClass;

function XYCurves_Get_Count(): Integer; CDECL;
function XYCurves_Get_First(): Integer; CDECL;
function XYCurves_Get_Name(): PAnsiChar; CDECL;
function XYCurves_Get_Next(): Integer; CDECL;
procedure XYCurves_Set_Name(const Value: PAnsiChar); CDECL;
function XYCurves_Get_Npts(): Integer; CDECL;
procedure XYCurves_Get_Xarray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure XYCurves_Get_Xarray_GR(); CDECL;
procedure XYCurves_Set_Npts(Value: Integer); CDECL;
procedure XYCurves_Set_Xarray(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function XYCurves_Get_x(): Double; CDECL;
function XYCurves_Get_y(): Double; CDECL;
procedure XYCurves_Get_Yarray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure XYCurves_Get_Yarray_GR(); CDECL;
procedure XYCurves_Set_x(Value: Double); CDECL;
procedure XYCurves_Set_y(Value: Double); CDECL;
procedure XYCurves_Set_Yarray(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function XYCurves_Get_Xscale(): Double; CDECL;
function XYCurves_Get_Xshift(): Double; CDECL;
function XYCurves_Get_Yscale(): Double; CDECL;
function XYCurves_Get_Yshift(): Double; CDECL;
procedure XYCurves_Set_Xscale(Value: Double); CDECL;
procedure XYCurves_Set_Xshift(Value: Double); CDECL;
procedure XYCurves_Set_Yscale(Value: Double); CDECL;
procedure XYCurves_Set_Yshift(Value: Double); CDECL;
procedure XYCurves_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure XYCurves_Get_AllNames_GR(); CDECL;

// API Extensions
function XYCurves_Get_idx(): Integer; CDECL;
procedure XYCurves_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSObject,
    SysUtils,
    DSSHelper;

function _activeObj(DSS: TDSSContext; out obj: TXYCurveObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
        
    obj := DSS.XYCurveClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['XYCurve'], 8989);
        end;
        Exit;
    end;
        
    Result := True;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    
    Result := DSSPrime.XYCurveClass.ElementCount;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    Result := DSSPrime.XYCurveClass.First;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Name(): PAnsiChar; CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(DSSPrime, pXYCurve) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, pXYCurve.Name);
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    
    Result := DSSPrime.XYCurveClass.Next;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Name(const Value: PAnsiChar); CDECL;
// set XYCurve active by name

begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if not DSSPrime.XYCurveClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'XYCurve "%s" not found in Active Circuit.', [Value], 51008);

    // Still same active object if not found
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Npts(): Integer; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51009);
        Exit;
    end;
    
    Result := pXYCurve.NumPoints;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_Xarray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pXYCurve: TXYCurveObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51013);
        Exit;
    end;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, pXYCurve.NumPoints);
    Move(pXYCurve.XValues[1], Result[0], pXYCurve.NumPoints * SizeOf(Double));
end;

procedure XYCurves_Get_Xarray_GR(); CDECL;
// Same as XYCurves_Get_Xarray but uses global result (GR) pointers
begin
    XYCurves_Get_Xarray(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure XYCurves_Set_Npts(Value: Integer); CDECL;
var
    pXYCurve: TXYCurveObj;
    prev: Integer;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51014);
        Exit;
    end;

    prev := pXYCurve.FNumPoints;
    pXYCurve.FNumPoints := Value;
    pXYCurve.PropertySideEffects(ord(TXYcurveProp.npts), prev);
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xarray(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pXYCurve: TXYCurveObj;
    ActualValueCount: TAPISize;
    Value: PDoubleArray0;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51015);
        Exit;
    end;

    if (pXYCurve.NumPoints <> ValueCount) and DSS_CAPI_EXT_ERRORS then
    begin
        DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, pXYCurve.NumPoints], 183);
        Exit;
    end;
    
    Value := PDoubleArray0(ValuePtr);
    // Only put in as many points as we have allocated
    ActualValueCount := ValueCount;
    if ActualValueCount > pXYCurve.NumPoints then
        ActualValueCount := pXYCurve.NumPoints;

    Move(Value[0], pXYCurve.XValues[1], ActualValueCount * SizeOf(Double));
end;
//------------------------------------------------------------------------------
function XYCurves_Get_x(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51010);
        Exit;
    end;
    
    Result := pXYCurve.X;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_y(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51011);
        Exit;
    end;
    
    Result := pXYCurve.Y;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_Yarray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    pXYCurve: TXYCurveObj;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51013);
        Exit;
    end;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, pXYCurve.NumPoints);
    Move(pXYCurve.YValues[1], ResultPtr^, pXYCurve.NumPoints * SizeOf(Double));
end;

procedure XYCurves_Get_Yarray_GR(); CDECL;
// Same as XYCurves_Get_Yarray but uses global result (GR) pointers
begin
    XYCurves_Get_Yarray(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure XYCurves_Set_x(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51010);
        Exit;
    end;

    pXYCurve.X := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_y(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51010);
        Exit;
    end;
    
    pXYCurve.Y := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yarray(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pXYCurve: TXYCurveObj;
    ActualValueCount: TAPISize;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51016);
        Exit;
    end;

    if (pXYCurve.NumPoints <> ValueCount) and DSS_CAPI_EXT_ERRORS then
    begin
        DoSimpleMsg(DSSPrime, 'The number of values provided (%d) does not match the expected (%d).', [ValueCount, pXYCurve.NumPoints], 183);
        Exit;
    end;
    
    // Only put in as many points as we have allocated
    ActualValueCount := ValueCount;
    if ActualValueCount > pXYCurve.NumPoints then
        ActualValueCount := pXYCurve.NumPoints;

    Move(ValuePtr^, pXYCurve.YValues[1], ActualValueCount * SizeOf(Double));
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Xscale(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51011);
        Exit;
    end;

    Result := pXYCurve.FXscale;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Xshift(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51011);
        Exit;
    end;
    
    Result := pXYCurve.FXshift;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Yscale(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51011);
        Exit;
    end;

    Result := pXYCurve.FYscale;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Yshift(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51011);
        Exit;
    end;

    Result := pXYCurve.FYshift;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xscale(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51010);
        Exit;
    end;
    
    pXYCurve.FXScale := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xshift(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51010);
        Exit;
    end;

    pXYCurve.FXShift := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yscale(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51010);
        Exit;
    end;

    pXYCurve.FYScale := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yshift(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(DSSPrime, pXYCurve) then
    begin
        DoSimpleMsg(DSSPrime, 'No active %s object found! Activate one and retry.', ['XYCurve'], 51010);
        Exit;
    end;

    pXYCurve.FYShift := Value;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_idx(): Integer; CDECL;
begin
    Result := DSSPrime.XYCurveClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_idx(Value: Integer); CDECL;
begin
    if (DSSPrime.XYCurveClass.ElementList.Get(Value) = NIL) then
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['XYCurve', Value], 656565);
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.XYCurveClass.ElementList, False);
end;

procedure XYCurves_Get_AllNames_GR(); CDECL;
// Same as XYCurves_Get_AllNames but uses global result (GR) pointers
begin
    XYCurves_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;
//------------------------------------------------------------------------------
end.
