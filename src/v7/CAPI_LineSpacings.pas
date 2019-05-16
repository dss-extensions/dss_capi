unit CAPI_LineSpacings;

{$inline on}

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
procedure LineSpacings_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LineSpacings_Get_Xcoords_GR(); CDECL;
procedure LineSpacings_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LineSpacings_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure LineSpacings_Get_Ycoords_GR(); CDECL;
procedure LineSpacings_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
procedure LineSpacings_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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

function LineSpacings_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := LineSpacingClass.ElementCount;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := LineSpacingClass.First;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := LineSpacingClass.Next;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Name_AnsiString(): Ansistring; inline;
var
    pLineSpacing: TLineSpacingObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        if pLineSpacing <> NIL then
        begin
            Result := pLineSpacing.Name;
        end;
    end;

end;

function LineSpacings_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(LineSpacings_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if not LineSpacingClass.SetActive(Value) then
            DoSimpleMsg('LineSpacing "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Nconds(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        Result := pLineSpacing.NWires;
    end
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Nconds(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;

begin

    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        if (Value < 1) then
        begin
            DoSimpleMsg('Invalid number of conductors sent via COM interface.  Please enter a value within range.', 183);
        end
        else
        begin
            pLineSpacing.DataChanged := TRUE;
            pLineSpacing.NWires := Value;
        end;
    end;
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Phases(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        Result := pLineSpacing.NPhases;
    end

end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Phases(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        with pLineSpacing do
        begin
            DataChanged := TRUE;
            NPhases := Value;
        end;
    end
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Set_Units(Value: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        with pLineSpacing do
        begin
            Units := Value;
            DataChanged := TRUE;
        end;
    end
end;
//------------------------------------------------------------------------------
function LineSpacings_Get_Units(): Integer; CDECL;
var
    pLineSpacing: TLineSpacingObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        with pLineSpacing do
        begin
            Result := Units;
        end;
    end
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Set_Ycoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
    i: Integer;
begin
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        with pLineSpacing do
        begin
            if NWires <> ValueCount then
            begin
                DoSimpleMsg('Invalid number of items sent via COM interface.  Please enter a value within range.', 183);
                Exit;
            end;

            for i := 1 to ValueCount do
                Ycoord[i] := ValuePtr[i - 1];

            DataChanged := TRUE;
        end;
    end
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Get_Ycoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pLineSpacing: TLineSpacingObj;
    i: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        with pLineSpacing do
        begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (NWires - 1) + 1);
            for i := 1 to NWires do
                Result[i - 1] := Ycoord[i];
        end;
    end
end;

procedure LineSpacings_Get_Ycoords_GR(); CDECL;
// Same as LineSpacings_Get_Ycoords but uses global result (GR) pointers
begin
    LineSpacings_Get_Ycoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Set_Xcoords(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    pLineSpacing: TLineSpacingObj;
    i: Integer;
begin
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        with pLineSpacing do
        begin
            if NWires <> ValueCount then
            begin
                DoSimpleMsg('Invalid number of items sent via COM interface.  Please enter a value within range.', 183);
                Exit;
            end;

            for i := 1 to ValueCount do
                Xcoord[i] := ValuePtr[i - 1];

            DataChanged := TRUE;
        end;
    end
end;
//------------------------------------------------------------------------------
procedure LineSpacings_Get_Xcoords(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pLineSpacing: TLineSpacingObj;
    i: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    if ActiveCircuit <> NIL then
    begin
        pLineSpacing := LineSpacingClass.GetActiveObj;
        with pLineSpacing do
        begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (NWires - 1) + 1);
            for i := 1 to NWires do
                Result[i - 1] := Xcoord[i];
        end;
    end
end;

procedure LineSpacings_Get_Xcoords_GR(); CDECL;
// Same as LineSpacings_Get_Xcoords but uses global result (GR) pointers
begin
    LineSpacings_Get_Xcoords(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure LineSpacings_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
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
