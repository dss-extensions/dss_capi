unit CAPI_LoadShapes;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function LoadShapes_Get_Name(): PAnsiChar; CDECL;
procedure LoadShapes_Set_Name(const Value: PAnsiChar); CDECL;
function LoadShapes_Get_Count(): Integer; CDECL;
function LoadShapes_Get_First(): Integer; CDECL;
function LoadShapes_Get_Next(): Integer; CDECL;
procedure LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure LoadShapes_Get_AllNames_GR(); CDECL;
function LoadShapes_Get_Npts(): Integer; CDECL;
procedure LoadShapes_Get_Pmult(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LoadShapes_Get_Pmult_GR(); CDECL;
procedure LoadShapes_Get_Qmult(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LoadShapes_Get_Qmult_GR(); CDECL;
procedure LoadShapes_Set_Npts(Value: Integer); CDECL;
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LoadShapes_Normalize(); CDECL;
procedure LoadShapes_Get_TimeArray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LoadShapes_Get_TimeArray_GR(); CDECL;
procedure LoadShapes_Set_TimeArray(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function LoadShapes_Get_HrInterval(): Double; CDECL;
function LoadShapes_Get_MinInterval(): Double; CDECL;
function LoadShapes_Get_SInterval(): Double; CDECL;
procedure LoadShapes_Set_HrInterval(Value: Double); CDECL;
procedure LoadShapes_Set_MinInterval(Value: Double); CDECL;
procedure LoadShapes_Set_SInterval(Value: Double); CDECL;
function LoadShapes_New(const Name: PAnsiChar): Integer; CDECL;
function LoadShapes_Get_PBase(): Double; CDECL;
function LoadShapes_Get_Qbase(): Double; CDECL;
procedure LoadShapes_Set_PBase(Value: Double); CDECL;
procedure LoadShapes_Set_Qbase(Value: Double); CDECL;
function LoadShapes_Get_UseActual(): TAPIBoolean; CDECL;
procedure LoadShapes_Set_UseActual(Value: TAPIBoolean); CDECL;

// API extensions
function LoadShapes_Get_idx(): Integer; CDECL;
procedure LoadShapes_Set_idx(Value: Integer); CDECL;
procedure LoadShapes_Set_Points(Npts: TAPISize; HoursPtr: Pointer; PMultPtr: Pointer; QMultPtr: Pointer; ExternalMemory: TAPIBoolean; IsFloat32: TAPIBoolean; Stride: Integer); CDECL;
procedure LoadShapes_UseFloat64(); CDECL;
procedure LoadShapes_UseFloat32(); CDECL;
procedure LoadShapes_Set_MaxP(Value: Double); CDECL;
function LoadShapes_Get_MaxP(): Double; CDECL;
procedure LoadShapes_Set_MaxQ(Value: Double); CDECL;
function LoadShapes_Get_MaxQ(): Double; CDECL;
function LoadShapes_Get_Pointer(): Pointer; CDECL;

implementation

uses
    CAPI_Constants,
    Loadshape,
    DSSGlobals,
    DSSPointerList,
    // ExecHelper,
    SysUtils,
    Math,
    DSSClass,
    DSSHelper,
    CAPI_Alt;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TLoadshapeObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.LoadshapeClass.GetActiveObj();
    if obj = NIL then
    begin
        DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Loadshape'], 61001);
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
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.LoadshapeClass.SetActive(Value) then
        Exit;
    DoSimpleMsg(DSSPrime, 'LoadShape "%s" not found in Active Circuit.', [Value], 77003);
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LoadshapeClass.ElementList.Count;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LoadshapeClass.First();
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LoadshapeClass.Next();
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.LoadShapeClass.ElementList, False);
end;

procedure LoadShapes_Get_AllNames_GR(); CDECL;
// Same as LoadShapes_Get_AllNames but uses global result (GR) pointers
begin
    LoadShapes_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function LoadShapes_Get_Npts(): Integer; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NumPoints;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Get_Pmult(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TLoadshapeObj;
    Result: PDoubleArray0;
    ActualNumPoints: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    if (not assigned(elem.dP)) and (not assigned(elem.sP)) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    ActualNumPoints := elem.NumPoints;
    elem.UseFloat64();
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.dP[0], ResultPtr[0], ActualNumPoints * SizeOf(Double));
end;

procedure LoadShapes_Get_Pmult_GR(); CDECL;
// Same as LoadShapes_Get_Pmult but uses global result (GR) pointers
begin
    LoadShapes_Get_Pmult(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Get_Qmult(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TLoadshapeObj;
    Result: PDoubleArray0;
    ActualNumPoints: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    if (not assigned(elem.dQ)) and (not assigned(elem.sQ)) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    elem.UseFloat64();
    ActualNumPoints := elem.NumPoints;
    DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.dQ[0], ResultPtr[0], ActualNumPoints * SizeOf(Double));
end;

procedure LoadShapes_Get_Qmult_GR(); CDECL;
// Same as LoadShapes_Get_Qmult but uses global result (GR) pointers
begin
    LoadShapes_Get_Qmult(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Set_Npts(Value: Integer); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.NumPoints := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.ExternalMemory then
    begin
        DoSimpleMsg(DSSPrime, _('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61101);
        Exit;
    end;
    // Only accept the new data when the number of points match
    if ValueCount <> elem.NumPoints then
    begin
        DoSimpleMsg(DSSPrime, 'The number of values (%d) does not match the current Npts (%d)!', [ValueCount, elem.NumPoints], 61100);
        Exit;
    end;
    ReallocMem(elem.sP, 0);
    elem.UseFloat64();
    ReallocMem(elem.dP, Sizeof(Double) * ValueCount);
    Move(ValuePtr[0], elem.dP[0], ValueCount * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.ExternalMemory then
    begin
        DoSimpleMsg(DSSPrime, _('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61101);
        Exit;
    end;
    
    // Only accept the new data when the number of points match
    if ValueCount <> elem.NumPoints then
    begin
        DoSimpleMsg(DSSPrime, 'The number of values (%d) does not match the current Npts (%d)!', [ValueCount, elem.NumPoints], 61101);
        Exit;
    end;
    ReallocMem(elem.sQ, 0);
    elem.UseFloat64();
    ReallocMem(elem.dQ, Sizeof(Double) * ValueCount);
    Move(ValuePtr[0], elem.dQ[0], ValueCount * SizeOf(Double));
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Normalize(); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Normalize;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Get_TimeArray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TLoadshapeObj;
    ActualNumPoints: Integer;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.UseFloat64();
    if elem.dH = NIL then
        Exit;
    ActualNumPoints := elem.NumPoints;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, ActualNumPoints);
    Move(elem.dH[0], ResultPtr[0], ActualNumPoints * SizeOf(Double));
end;

procedure LoadShapes_Get_TimeArray_GR(); CDECL;
// Same as LoadShapes_Get_TimeArray but uses global result (GR) pointers
begin
    LoadShapes_Get_TimeArray(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Set_TimeArray(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.ExternalMemory then
    begin
        DoSimpleMsg(DSSPrime, _('Data cannot be changed for LoadShapes with external memory! Reset the data first.'), 61101);
        Exit;
    end;
    
    // Only accept the new data when the number of points match
    if ValueCount <> elem.NumPoints then
    begin
        DoSimpleMsg(DSSPrime, 'The number of values (%d) does not match the current Npts (%d)!', [ValueCount, elem.NumPoints], 61102);
        Exit;
    end;
    ReallocMem(elem.sH, 0);
    elem.UseFloat64();
    ReallocMem(elem.dH, Sizeof(Double) * ValueCount);        
    Move(ValuePtr[0], elem.dH[0], ValueCount * SizeOf(Double));
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_HrInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Interval;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_MinInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Interval * 60.0;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_SInterval(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Interval * 3600.0;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_HrInterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Interval := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MinInterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Interval := Value / 60.0;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_SInterval(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Interval := Value / 3600.0;
end;
//------------------------------------------------------------------------------
function LoadShapes_New(const Name: PAnsiChar): Integer; CDECL;
begin
    DSSPrime.LoadShapeClass.NewObject(Name, True, Result);
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_PBase(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.baseP;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Qbase(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.baseQ;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_PBase(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.baseP := Value;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qbase(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.baseQ := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_UseActual(): TAPIBoolean; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.UseActual;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_UseActual(Value: TAPIBoolean); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.UseActual := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_idx(): Integer; CDECL;
begin
    Result := DSSPrime.LoadShapeClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_idx(Value: Integer); CDECL;
begin
    if DSSPrime.LoadShapeClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['LoadShape', Value], 656565);
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Points(Npts: TAPISize; HoursPtr: Pointer; PMultPtr: Pointer; QMultPtr: Pointer; ExternalMemory: TAPIBoolean; IsFloat32: TAPIBoolean; Stride: Integer); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(DSSPrime, elem) then
        Exit;

    Alt_LoadShape_Set_Points(elem, Npts, HoursPtr, PMultPtr, QMultPtr, ExternalMemory, IsFloat32, Stride);
end;
//------------------------------------------------------------------------------
procedure LoadShapes_UseFloat64(); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.UseFloat64();
end;
//------------------------------------------------------------------------------
procedure LoadShapes_UseFloat32(); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.UseFloat32();
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MaxP(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.MaxP := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_MaxP(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
	if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MaxP;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MaxQ(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.MaxQ := Value;
    elem.MaxQSpecified := True;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_MaxQ(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
	if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MaxQ;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LoadshapeClass.GetActiveObj();
end;
//------------------------------------------------------------------------------
end.
