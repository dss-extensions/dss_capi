unit CAPI_XYCurves;

interface

uses
    CAPI_Utils,
    XYCurve,
    DSSClass;

function XYCurves_Get_Count(): Integer; CDECL;
function XYCurves_Get_First(): Integer; CDECL;
function XYCurves_Get_Name(): PAnsiChar; CDECL;
function XYCurves_Get_Next(): Integer; CDECL;
procedure XYCurves_Set_Name(const Value: PAnsiChar); CDECL;
function XYCurves_Get_Npts(): Integer; CDECL;
procedure XYCurves_Get_Xarray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure XYCurves_Get_Xarray_GR(); CDECL;
procedure XYCurves_Set_Npts(Value: Integer); CDECL;
procedure XYCurves_Set_Xarray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function XYCurves_Get_x(): Double; CDECL;
function XYCurves_Get_y(): Double; CDECL;
procedure XYCurves_Get_Yarray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure XYCurves_Get_Yarray_GR(); CDECL;
procedure XYCurves_Set_x(Value: Double); CDECL;
procedure XYCurves_Set_y(Value: Double); CDECL;
procedure XYCurves_Set_Yarray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function XYCurves_Get_Xscale(): Double; CDECL;
function XYCurves_Get_Xshift(): Double; CDECL;
function XYCurves_Get_Yscale(): Double; CDECL;
function XYCurves_Get_Yshift(): Double; CDECL;
procedure XYCurves_Set_Xscale(Value: Double); CDECL;
procedure XYCurves_Set_Xshift(Value: Double); CDECL;
procedure XYCurves_Set_Yscale(Value: Double); CDECL;
procedure XYCurves_Set_Yshift(Value: Double); CDECL;
procedure XYCurves_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure XYCurves_Get_AllNames_GR(); CDECL;

// API Extensions
function XYCurves_Get_idx(): Integer; CDECL;
procedure XYCurves_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSObject,
    SysUtils;

function _activeObj(out obj: TXYCurveObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
        
    obj := XYCurveClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active XYCurve object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
        
    Result := True;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    
    Result := XYCurveClass.ElementCount;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
        
    Result := XYCurveClass.First;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Name(): PAnsiChar; CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(pXYCurve) then
        Exit;

    Result := DSS_GetAsPAnsiChar(pXYCurve.Name);
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    
    Result := XYCurveClass.Next;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Name(const Value: PAnsiChar); CDECL;
// set XYCurve active by name

begin
    if InvalidCircuit then
        Exit;

    if not XYCurveClass.SetActive(Value) then
        DoSimpleMsg('XYCurve "' + Value + '" Not Found in Active Circuit.', 51008);

    // Still same active object if not found
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Npts(): Integer; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51009);
        Exit;
    end;
    
    Result := pXYCurve.NumPoints;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_Xarray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pXYCurve: TXYCurveObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51013);
        Exit;
    end;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, pXYCurve.NumPoints);
    Move(pXYCurve.XValues[1], Result[0], pXYCurve.NumPoints * SizeOf(Double));
end;

procedure XYCurves_Get_Xarray_GR(); CDECL;
// Same as XYCurves_Get_Xarray but uses global result (GR) pointers
begin
    XYCurves_Get_Xarray(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure XYCurves_Set_Npts(Value: Integer); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51014);
        Exit;
    end;

    pXYCurve.NumPoints := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xarray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    pXYCurve: TXYCurveObj;
    ActualValueCount: Integer;
    Value: PDoubleArray;
begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51015);
        Exit;
    end;

    if (pXYCurve.NumPoints <> ValueCount) and DSS_CAPI_EXT_ERRORS then
    begin
        DoSimpleMsg(Format('The number of values provided (%d) does not match the expected (%d).', [ValueCount, pXYCurve.NumPoints]), 183);
        Exit;
    end;
    
    Value := PDoubleArray(ValuePtr);
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
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51010);
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
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51011);
        Exit;
    end;
    
    Result := pXYCurve.Y;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_Yarray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pXYCurve: TXYCurveObj;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51013);
        Exit;
    end;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, pXYCurve.NumPoints);
    Move(pXYCurve.YValues[1], ResultPtr^, pXYCurve.NumPoints * SizeOf(Double));
end;

procedure XYCurves_Get_Yarray_GR(); CDECL;
// Same as XYCurves_Get_Yarray but uses global result (GR) pointers
begin
    XYCurves_Get_Yarray(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure XYCurves_Set_x(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51010);
        Exit;
    end;

    pXYCurve.X := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_y(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51010);
        Exit;
    end;
    
    pXYCurve.Y := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yarray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    pXYCurve: TXYCurveObj;
    ActualValueCount: Integer;
begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51016);
        Exit;
    end;

    if (pXYCurve.NumPoints <> ValueCount) and DSS_CAPI_EXT_ERRORS then
    begin
        DoSimpleMsg(Format('The number of values provided (%d) does not match the expected (%d).', [ValueCount, pXYCurve.NumPoints]), 183);
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
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51011);
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
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51011);
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
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51011);
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
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51011);
        Exit;
    end;

    Result := pXYCurve.FYshift;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xscale(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51010);
        Exit;
    end;
    
    pXYCurve.FXScale := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xshift(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51010);
        Exit;
    end;

    pXYCurve.FXShift := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yscale(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51010);
        Exit;
    end;

    pXYCurve.FYScale := Value;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yshift(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;
begin
    if not _activeObj(pXYCurve) then
    begin
        DoSimpleMsg('No active XYCurve Object found.', 51010);
        Exit;
    end;

    pXYCurve.FYShift := Value;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_idx(): Integer; CDECL;
begin
    Result := XYCurveClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_idx(Value: Integer); CDECL;
begin
    if (XYCurveClass.ElementList.Get(Value) = NIL) then
        DoSimpleMsg('Invalid XYCurve index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    if InvalidCircuit then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
        Result[0] := DSS_CopyStringAsPChar('NONE');
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, XYCurveClass.ElementList, False);
end;

procedure XYCurves_Get_AllNames_GR(); CDECL;
// Same as XYCurves_Get_AllNames but uses global result (GR) pointers
begin
    XYCurves_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
end.
