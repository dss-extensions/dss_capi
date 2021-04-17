unit CAPI_LoadShapes;

interface

uses
    CAPI_Utils;

function LoadShapes_Get_Name(): PAnsiChar; CDECL;
procedure LoadShapes_Set_Name(const Value: PAnsiChar); CDECL;
function LoadShapes_Get_Count(): Integer; CDECL;
function LoadShapes_Get_First(): Integer; CDECL;
function LoadShapes_Get_Next(): Integer; CDECL;
procedure LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure LoadShapes_Get_AllNames_GR(); CDECL;
function LoadShapes_Get_Npts(): Integer; CDECL;
procedure LoadShapes_Get_Pmult(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LoadShapes_Get_Pmult_GR(); CDECL;
procedure LoadShapes_Get_Qmult(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LoadShapes_Get_Qmult_GR(); CDECL;
procedure LoadShapes_Set_Npts(Value: Integer); CDECL;
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LoadShapes_Normalize(); CDECL;
procedure LoadShapes_Get_TimeArray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LoadShapes_Get_TimeArray_GR(); CDECL;
procedure LoadShapes_Set_TimeArray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function LoadShapes_Get_HrInterval(): Double; CDECL;
function LoadShapes_Get_MinInterval(): Double; CDECL;
function LoadShapes_Get_sInterval(): Double; CDECL;
procedure LoadShapes_Set_HrInterval(Value: Double); CDECL;
procedure LoadShapes_Set_MinInterval(Value: Double); CDECL;
procedure LoadShapes_Set_Sinterval(Value: Double); CDECL;
function LoadShapes_New(const Name: PAnsiChar): Integer; CDECL;
function LoadShapes_Get_PBase(): Double; CDECL;
function LoadShapes_Get_Qbase(): Double; CDECL;
procedure LoadShapes_Set_PBase(Value: Double); CDECL;
procedure LoadShapes_Set_Qbase(Value: Double); CDECL;
function LoadShapes_Get_UseActual(): Wordbool; CDECL;
procedure LoadShapes_Set_UseActual(Value: Wordbool); CDECL;

// API extensions
function LoadShapes_Get_idx(): Integer; CDECL;
procedure LoadShapes_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    Loadshape,
    DSSGlobals,
    PointerList,
    ExecHelper,
    SysUtils,
    Math;

//------------------------------------------------------------------------------
function _activeObj(out obj: TLoadshapeObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := LoadshapeClass.GetActiveObj;
    if obj = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61001);
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Name(): PAnsiChar; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := NIL;
    if not _activeObj(elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem.Name);
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit then
        Exit;
    if LoadshapeClass.SetActive(Value) then
        Exit;
    DoSimpleMsg('LoadShape "' + Value + '" Not Found in Active Circuit.', 77003);
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := LoadshapeClass.ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := LoadshapeClass.First;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := LoadshapeClass.Next;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, LoadShapeClass.ElementList, False);
end;

procedure LoadShapes_Get_AllNames_GR(); CDECL;
// Same as LoadShapes_Get_AllNames but uses global result (GR) pointers
begin
    LoadShapes_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function LoadShapes_Get_Npts(): Integer; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.NumPoints;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Get_Pmult(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    elem: TLoadshapeObj;
    Result: PDoubleArray;
    ActualNumPoints: Integer;
begin
    if not _activeObj(elem) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    if not assigned(elem.PMultipliers) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    ActualNumPoints := elem.NumPoints;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.PMultipliers^[1], ResultPtr[0], ActualNumPoints * SizeOf(Double));
end;

procedure LoadShapes_Get_Pmult_GR(); CDECL;
// Same as LoadShapes_Get_Pmult but uses global result (GR) pointers
begin
    LoadShapes_Get_Pmult(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Get_Qmult(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    elem: TLoadshapeObj;
    Result: PDoubleArray;
    ActualNumPoints: Integer;
begin
    if not _activeObj(elem) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    if not assigned(elem.QMultipliers) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
        Exit;
    end;
    ActualNumPoints := elem.NumPoints;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.QMultipliers^[1], ResultPtr[0], ActualNumPoints * SizeOf(Double));
end;

procedure LoadShapes_Get_Qmult_GR(); CDECL;
// Same as LoadShapes_Get_Qmult but uses global result (GR) pointers
begin
    LoadShapes_Get_Qmult(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Set_Npts(Value: Integer); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.NumPoints := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;

    with elem do
    begin
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61100);
            Exit;
        end;

        ReallocMem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
        Move(ValuePtr[0], PMultipliers^[1], ValueCount * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;

    with elem do
    begin
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61101);
            Exit;
        end;

        ReallocMem(QMultipliers, Sizeof(QMultipliers^[1]) * NumPoints);
        Move(ValuePtr[0], QMultipliers^[1], ValueCount * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Normalize(); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Normalize;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Get_TimeArray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    elem: TLoadshapeObj;
    Result: PDoubleArray;
    ActualNumPoints: Integer;

begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    if not _activeObj(elem) then
        Exit;

    if elem.Hours = NIL then
        Exit;
    ActualNumPoints := elem.NumPoints;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.Hours^[1], ResultPtr[0], ActualNumPoints * SizeOf(Double));
end;

procedure LoadShapes_Get_TimeArray_GR(); CDECL;
// Same as LoadShapes_Get_TimeArray but uses global result (GR) pointers
begin
    LoadShapes_Get_TimeArray(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Set_TimeArray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;

    with elem do
    begin
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61102);
            Exit;
        end;

        ReallocMem(Hours, Sizeof(Hours^[1]) * NumPoints);
        Move(ValuePtr[0], Hours[0], ValueCount * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_HrInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.Interval;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_MinInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.Interval * 60.0;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_sInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.Interval * 3600.0;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_HrInterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Interval := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MinInterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Interval := Value / 60.0;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Sinterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.Interval := Value / 3600.0;
end;
//------------------------------------------------------------------------------
function LoadShapes_New(const Name: PAnsiChar): Integer; CDECL;
begin
    Result := AddObject('loadshape', Name);    // Returns handle to object
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_PBase(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.baseP;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Qbase(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(elem) then
        Exit;
    Result := elem.baseQ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_PBase(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.baseP := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qbase(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.baseQ := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_UseActual(): Wordbool; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := elem.UseActual;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_UseActual(Value: Wordbool); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;
    elem.UseActual := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_idx(): Integer; CDECL;
begin
    Result := LoadShapeClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_idx(Value: Integer); CDECL;
begin
    if LoadShapeClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg('Invalid LoadShape index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
