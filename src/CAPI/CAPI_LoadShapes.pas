unit CAPI_LoadShapes;

interface

uses
    CAPI_Utils;

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
function LoadShapes_Get_sInterval(): Double; CDECL;
procedure LoadShapes_Set_HrInterval(Value: Double); CDECL;
procedure LoadShapes_Set_MinInterval(Value: Double); CDECL;
procedure LoadShapes_Set_Sinterval(Value: Double); CDECL;
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

implementation

uses
    CAPI_Constants,
    Loadshape,
    DSSGlobals,
    DSSPointerList,
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
    Result := LoadshapeClass.ElementList.Count;
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
procedure LoadShapes_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
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
procedure LoadShapes_Get_Pmult(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TLoadshapeObj;
    Result: PDoubleArray;
    ActualNumPoints: Integer;
begin
    if not _activeObj(elem) then
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
    LoadShapes_Get_Pmult(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Get_Qmult(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TLoadshapeObj;
    Result: PDoubleArray;
    ActualNumPoints: Integer;
begin
    if not _activeObj(elem) then
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
procedure LoadShapes_Set_Pmult(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;

    with elem do
    begin
        if elem.ExternalMemory then
        begin
            DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61101);
            Exit;
        end;
    
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61100);
            Exit;
        end;
        ReallocMem(sP, 0);
        UseFloat64();
        ReallocMem(dP, Sizeof(Double) * ValueCount);
        Move(ValuePtr[0], dP[0], ValueCount * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_Qmult(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;

    with elem do
    begin
        if ExternalMemory then
        begin
            DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61101);
            Exit;
        end;
        
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61101);
            Exit;
        end;
        ReallocMem(sQ, 0);
        UseFloat64;
        ReallocMem(dQ, Sizeof(Double) * ValueCount);
        Move(ValuePtr[0], dQ[0], ValueCount * SizeOf(Double));
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
procedure LoadShapes_Get_TimeArray(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TLoadshapeObj;
    ActualNumPoints: Integer;
begin
    DefaultResult(ResultPtr, ResultCount);
    if not _activeObj(elem) then
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
    LoadShapes_Get_TimeArray(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LoadShapes_Set_TimeArray(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TLoadshapeObj;
begin
    if not _activeObj(elem) then
        Exit;

    with elem do
    begin
        if elem.ExternalMemory then
        begin
            DoSimpleMsg('Data cannot be changed for LoadShapes with external memory! Reset the data first.', 61101);
            Exit;
        end;
        
        // Only accept the new data when the number of points match
        if ValueCount <> NumPoints then
        begin
            DoSimpleMsg(Format('The number of values (%d) does not match the current Npts (%d)!', [ValueCount, NumPoints]), 61102);
            Exit;
        end;
        ReallocMem(sH, 0);
        UseFloat64;
        ReallocMem(dH, Sizeof(Double) * ValueCount);        
        Move(ValuePtr[0], dH[0], ValueCount * SizeOf(Double));
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
function LoadShapes_Get_UseActual(): TAPIBoolean; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := FALSE;
    if not _activeObj(elem) then
        Exit;
    Result := elem.UseActual;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_UseActual(Value: TAPIBoolean); CDECL;
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
procedure LoadShapes_Set_Points(Npts: TAPISize; HoursPtr: Pointer; PMultPtr: Pointer; QMultPtr: Pointer; ExternalMemory: TAPIBoolean; IsFloat32: TAPIBoolean; Stride: Integer); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(elem) then
        Exit;

    // If the LoadShape owns the memory, dispose the current data and reallocate if necessary
    if not elem.ExternalMemory then
    begin
        ReallocMem(elem.dP, 0);
        ReallocMem(elem.dQ, 0);
        ReallocMem(elem.dH, 0);
        ReallocMem(elem.sP, 0);
        ReallocMem(elem.sQ, 0);
        ReallocMem(elem.sH, 0);
    end;
    elem.dP := NIL;
    elem.dQ := NIL;
    elem.dH := NIL;
    elem.sP := NIL;
    elem.sQ := NIL;
    elem.sH := NIL;
    
    elem.ExternalMemory := ExternalMemory;
    elem.NumPoints := Npts;

    if not ExternalMemory then
    begin
        elem.Stride := 1;
        if not IsFloat32 then
        begin
            if PMultPtr <> NIL then
            begin
                ReallocMem(elem.dP, Sizeof(Double) * Npts);
                Move(PMultPtr^, elem.dP[0], Npts * SizeOf(Double));
            end;
            if QMultPtr <> NIL then
            begin
                ReallocMem(elem.dQ, Sizeof(Double) * Npts);
                Move(QMultPtr^, elem.dQ[0], Npts * SizeOf(Double));
            end;
            if HoursPtr <> NIL then
            begin
                ReallocMem(elem.dH, Sizeof(Double) * Npts);
                Move(HoursPtr^, elem.dH[0], Npts * SizeOf(Double));
            end;
            if Assigned(elem.dP) then
                elem.SetMaxPandQ;
        end
        else // if IsFloat32
        begin
            if PMultPtr <> NIL then
            begin
                ReallocMem(elem.sP, Sizeof(Single) * Npts);
                Move(PMultPtr^, elem.sP[0], Npts * SizeOf(Single));
            end;
            if QMultPtr <> NIL then
            begin
                ReallocMem(elem.sQ, Sizeof(Single) * Npts);
                Move(QMultPtr^, elem.sQ[0], Npts * SizeOf(Single));
            end;
            if HoursPtr <> NIL then
            begin
                ReallocMem(elem.sH, Sizeof(Single) * Npts);
                Move(HoursPtr^, elem.sH[0], Npts * SizeOf(Single));
            end;
            if Assigned(elem.sP) then
                elem.SetMaxPandQ;
        end;
        Exit;
    end;
    
    // Using externally controlled memory
    if not IsFloat32 then
        elem.SetDataPointers(HoursPtr, PMultPtr, QMultPtr, Stride)
    else
        elem.SetDataPointersSingle(HoursPtr, PMultPtr, QMultPtr, Stride)
end;
//------------------------------------------------------------------------------
procedure LoadShapes_UseFloat64(); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(elem) then
        Exit;
    elem.UseFloat64();
end;
//------------------------------------------------------------------------------
procedure LoadShapes_UseFloat32(); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(elem) then
        Exit;
    elem.UseFloat32();
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MaxP(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(elem) then
        Exit;
    elem.MaxP := Value;
end;
//------------------------------------------------------------------------------
function LoadShapes_Get_MaxP(): Double; CDECL;
var
    elem: TLoadshapeObj;
begin
    Result := 0.0;
	if not _activeObj(elem) then
        Exit;
    Result := elem.MaxP;
end;
//------------------------------------------------------------------------------
procedure LoadShapes_Set_MaxQ(Value: Double); CDECL;
var
    elem: TLoadshapeObj;
begin
	if not _activeObj(elem) then
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
	if not _activeObj(elem) then
        Exit;
    Result := elem.MaxQ;
end;
//------------------------------------------------------------------------------
end.
