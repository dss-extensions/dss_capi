unit CAPI_LineSpacings;

interface

uses
    CAPI_Utils,
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
    ParserDel,
    Ucomplex,
    Line,
    UcMatrix;

//------------------------------------------------------------------------------
function _activeObj(out obj: TLineSpacingObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := LineSpacingClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active LineSpacing object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := LineSpacingClass.ElementCount;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := LineSpacingClass.First;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := LineSpacingClass.Next;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Name(): PAnsiChar; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(pLineSpacing) then
        Exit;
        
    Result := DSS_GetAsPAnsiChar(pLineSpacing.Name);
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;

    if not LineSpacingClass.SetActive(Value) then
        DoSimpleMsg('LineSpacing "' + Value + '" Not Found in Active Circuit.', 51008);

    // Still same active object if not found
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Nconds(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if not _activeObj(pLineSpacing) then
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
        DoSimpleMsg(Format('Invalid number of conductors (%d) sent via C-API. Please use a value within the valid range (>0).', [Value]), 183);
    end;
    if not _activeObj(pLineSpacing) then
        Exit;
    pLineSpacing.DataChanged := TRUE;
    pLineSpacing.NWires := Value;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Phases(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if not _activeObj(pLineSpacing) then
        Exit;
        
    Result := pLineSpacing.NPhases;
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Phases(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(pLineSpacing) then
        Exit;
        
    pLineSpacing.DataChanged := TRUE;
    pLineSpacing.NPhases := Value;
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Set_Units(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(pLineSpacing) then
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
    if not _activeObj(pLineSpacing) then
        Exit;

    Result := pLineSpacing.Units;
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Ycoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(pLineSpacing) then
        Exit;

    with pLineSpacing do
    begin
        if NWires <> ValueCount then
        begin
            DoSimpleMsg(Format(
                'The number of values provided (%d) does not match the number of wires (%d).', 
                [ValueCount, NWires]
            ), 183);
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
    if not _activeObj(pLineSpacing) then
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
    LineSpacings_Get_Ycoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Set_Xcoords(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if not _activeObj(pLineSpacing) then
        Exit;

    with pLineSpacing do
    begin
        if NWires <> ValueCount then
        begin
            DoSimpleMsg(Format(
                'The number of values provided (%d) does not match the number of wires (%d).', 
                [ValueCount, NWires]
            ), 183);
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
    if not _activeObj(pLineSpacing) then
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
    LineSpacings_Get_Xcoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, LineSpacingClass.ElementList, False);
end;

procedure LineSpacings_Get_AllNames_GR(); CDECL;
// Same as LineSpacings_Get_AllNames but uses global result (GR) pointers
begin
    LineSpacings_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_idx(): Integer; CDECL;
begin
    Result := LineSpacingClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_idx(Value: Integer); CDECL;
begin
    if LineSpacingClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg('Invalid LineSpacing index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
