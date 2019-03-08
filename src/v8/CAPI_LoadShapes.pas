unit CAPI_LoadShapes;

{$inline on}

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

function LoadShapes_Get_Name_AnsiString(): Ansistring; inline;
var
    elem: TLoadshapeObj;
begin
    Result := '';
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
        Exit;
    Result := elem.Name;
end;

function LoadShapes_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(LoadShapes_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    if LoadShapeClass[ActiveActor].SetActive(Value) then
        Exit;
    DoSimpleMsg('LoadShape "' + Value + '" Not Found in Active Circuit.', 77003);
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Result := LoadShapeClass[ActiveActor].ElementList.ListSize;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Result := LoadShapeClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Result := LoadShapeClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    elem: TLoadshapeObj;
    pList: TPointerList;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    if LoadShapeClass[ActiveActor].ElementList.ListSize > 0 then
    begin
        pList := LoadShapeClass[ActiveActor].ElementList;
        DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (pList.ListSize - 1) + 1);
        k := 0;
        elem := pList.First;
        while elem <> NIL do
        begin
            Result[k] := DSS_CopyStringAsPChar(elem.Name);
            Inc(k);
            elem := pList.next;
        end;
    end;
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
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
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
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61001);
        Exit;
    end;
    ActualNumPoints := Min(Length(elem.PMultipliers), elem.NumPoints);
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.PMultipliers[0], ResultPtr[0], ActualNumPoints * SizeOf(Double));
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
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61001);
    end;
    if not assigned(elem.QMultipliers) then
        Exit;
    ActualNumPoints := Min(Length(elem.QMultipliers), elem.NumPoints);
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.QMultipliers[0], ResultPtr[0], ActualNumPoints * SizeOf(Double));
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
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61001);
        Exit;
    end;
    elem.NumPoints := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    elem: TLoadshapeObj;
    Value: PDoubleArray;
    i, k, LoopLimit: Integer;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61002);
        Exit;
    end;

    Value := PDoubleArray(ValuePtr);
    with elem do
    begin
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61100);
            Exit;
        end;

        SetLength(PMultipliers, 0);
        SetLength(PMultipliers, ValueCount);
        Move(ValuePtr[0], PMultipliers[0], ValueCount * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    elem: TLoadshapeObj;
    Value: PDoubleArray;
begin
    Value := PDoubleArray(ValuePtr);
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61002);
        Exit;
    end;

    with elem do
    begin
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61101);
            Exit;
        end;

        SetLength(QMultipliers, 0);
        SetLength(QMultipliers, ValueCount);
        Move(ValuePtr[0], QMultipliers[0], ValueCount * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Normalize(); CDECL;
var
    elem: TLoadshapeObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61003);
        Exit;
    end;
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
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61003);
        Exit;
    end;

    if elem.Hours = NIL then
        Exit;
    ActualNumPoints := Min(Length(elem.Hours), elem.NumPoints);
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.Hours[0], ResultPtr[0], ActualNumPoints * SizeOf(Double));
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
    Value: PDoubleArray;
begin
    Value := PDoubleArray(ValuePtr);
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61002);
        Exit;
    end;
    with elem do
    begin
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61102);
            Exit;
        end;

        SetLength(Hours, 0);
        SetLength(Hours, ValueCount);
        Move(ValuePtr[0], Hours[0], ValueCount * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_HrInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    Result := elem.Interval;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_MinInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    Result := elem.Interval * 60.0;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_sInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    Result := elem.Interval * 3600.0;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_HrInterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    elem.Interval := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MinInterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    elem.Interval := Value / 60.0;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Sinterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
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
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    Result := elem.baseP;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Qbase(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    Result := elem.baseQ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_PBase(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    elem.baseP := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qbase(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    elem.baseQ := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_UseActual(): Wordbool; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := FALSE;
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    Result := elem.UseActual;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_UseActual(Value: Wordbool); CDECL;
var
    elem: TLoadshapeObj;
begin
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    elem := LoadShapeClass[ActiveActor].GetActiveObj;
    if elem = NIL then
    begin
        DoSimpleMsg('No active Loadshape Object found.', 61005);
        Exit;
    end;
    elem.UseActual := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_idx(): Integer; CDECL;
begin
    Result := LoadShapeClass[ActiveActor].ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_idx(Value: Integer); CDECL;
begin
    if LoadShapeClass[ActiveActor].ElementList.Get(Value) = NIL then
        DoSimpleMsg('Invalid LoadShape index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
