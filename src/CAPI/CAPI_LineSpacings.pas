unit CAPI_LineSpacings;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    LineSpacing;

function LineSpacings_Get_Count(): Integer; CDECL;
function LineSpacings_Get_First(): Integer; CDECL;
function LineSpacings_Get_Next(): Integer; CDECL;
function LineSpacings_Get_Name(): PAnsiChar; CDECL;
procedure LineSpacings_Set_Name(const Value: PAnsiChar); CDECL;
function LineSpacings_Get_Nconds(): Integer; CDECL;
procedure LineSpacings_Set_Nconds(Value: Integer); CDECL;
function LineSpacings_Get_Phases(): Integer; CDECL;
procedure LineSpacings_Set_Phases(Value: Integer); CDECL;
function LineSpacings_Get_Units(): Integer; CDECL;
procedure LineSpacings_Set_Units(Value: Integer); CDECL;
procedure LineSpacings_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LineSpacings_Get_Xcoords_GR(); CDECL;
procedure LineSpacings_Set_Xcoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LineSpacings_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure LineSpacings_Get_Ycoords_GR(); CDECL;
procedure LineSpacings_Set_Ycoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
procedure LineSpacings_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure LineSpacings_Get_AllNames_GR(); CDECL;

function LineSpacings_Get_idx(): Integer; CDECL;
procedure LineSpacings_Set_idx(Value: Integer); CDECL;


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
function _activeObj(DSS: TDSSContext; out obj: TLineSpacingObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.LineSpacingClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['LineSpacing'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineSpacingClass.ElementCount;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineSpacingClass.First;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.LineSpacingClass.Next;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Name(): PAnsiChar; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;
        
    Result := DSS_GetAsPAnsiChar(DSSPrime, pLineSpacing.Name);
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if not DSSPrime.LineSpacingClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'LineSpacing "%s" not found in Active Circuit.', [Value], 51008);

    // Still same active object if not found
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Nconds(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;
        
    Result := pLineSpacing.NWires;
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Nconds(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if (Value < 1) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid number of conductors (%d) sent via C-API. Please use a value within the valid range (>0).', [Value], 183);
    end;
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;
    pLineSpacing.FNConds := Value;
    pLineSpacing.PropertySideEffects(ord(TLineSpacingProp.NConds), 0);
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Phases(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;
        
    Result := pLineSpacing.NPhases;
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Phases(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;
        
    pLineSpacing.DataChanged := TRUE;
    pLineSpacing.NPhases := Value;
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Set_Units(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;

    pLineSpacing.Units := Value;
    pLineSpacing.DataChanged := TRUE;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Units(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;

    Result := pLineSpacing.Units;
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Ycoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;

    with pLineSpacing do
    begin
        if NWires <> ValueCount then
        begin
            DoSimpleMsg(
                'The number of values provided (%d) does not match the number of wires (%d).', 
                [ValueCount, NWires],
            183);
            Exit;
        end;
        Move(ValuePtr^, FY[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(DSSPrime, pLineSpacing) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    
    with pLineSpacing do
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NWires);
        Move(FY[1], ResultPtr^, ResultCount^ * SizeOf(Double));
    end;
end;

procedure LineSpacings_Get_Ycoords_GR(); CDECL;
// Same as LineSpacings_Get_Ycoords but uses global result (GR) pointers
begin
    LineSpacings_Get_Ycoords(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Set_Xcoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(DSSPrime, pLineSpacing) then
        Exit;

    with pLineSpacing do
    begin
        if NWires <> ValueCount then
        begin
            DoSimpleMsg(
                'The number of values provided (%d) does not match the number of wires (%d).', 
                [ValueCount, NWires],
            183);
            Exit;
        end;
        Move(ValuePtr^, FX[1], ValueCount * SizeOf(Double));
        DataChanged := TRUE;
    end;
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(DSSPrime, pLineSpacing) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    with pLineSpacing do
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NWires);
        Move(FX[1], ResultPtr^, ResultCount^ * SizeOf(Double));
    end;
end;

procedure LineSpacings_Get_Xcoords_GR(); CDECL;
// Same as LineSpacings_Get_Xcoords but uses global result (GR) pointers
begin
    LineSpacings_Get_Xcoords(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.LineSpacingClass.ElementList, False);
end;

procedure LineSpacings_Get_AllNames_GR(); CDECL;
// Same as LineSpacings_Get_AllNames but uses global result (GR) pointers
begin
    LineSpacings_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_idx(): Integer; CDECL;
begin
    Result := DSSPrime.LineSpacingClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_idx(Value: Integer); CDECL;
begin
    if DSSPrime.LineSpacingClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['LineSpacing', Value], 656565);
end;
//------------------------------------------------------------------------------
end.
