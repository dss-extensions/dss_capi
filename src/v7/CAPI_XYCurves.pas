unit CAPI_XYCurves;

{$inline on}

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

function XYCurves_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := XYCurveClass.ElementCount;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := XYCurveClass.First;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Name_AnsiString(): Ansistring; inline;
var
    pXYCurve: TXYCurveObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.Name;
        end;
    end;

end;

function XYCurves_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(XYCurves_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function XYCurves_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := XYCurveClass.Next;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Name(const Value: PAnsiChar); CDECL;
// set XYCurve active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if not XYCurveClass.SetActive(Value) then
            DoSimpleMsg('XYCurve "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_Npts(): Integer; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.NumPoints;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51009);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_Xarray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pXYCurve: TXYCurveObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (pXYCurve.NumPoints - 1) + 1);
            for k := 0 to pXYCurve.NumPoints - 1 do
                Result[k] := pXYCurve.XValue_pt[k + 1];
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51013);
        end;
    end;
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
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            pXYCurve.NumPoints := Value;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51014);
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xarray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pXYCurve: TXYCurveObj;
    i, k, LoopLimit: Integer;

begin
    Value := PDoubleArray(ValuePtr);
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin

        // Only put in as many points as we have allocated
            LoopLimit := (ValueCount - 1);
            if (LoopLimit - (0) + 1) > pXYCurve.NumPoints then
                LoopLimit := (0) + pXYCurve.NumPoints - 1;

            k := 1;
            for i := (0) to LoopLimit do
            begin
                pXYCurve.XValue_pt[k] := Value[i];
                inc(k);
            end;

        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51015);
        end;
    end;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_x(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.X;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51010);
        end;
    end;
end;
//------------------------------------------------------------------------------
function XYCurves_Get_y(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.Y;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51011);
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_Yarray(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    pXYCurve: TXYCurveObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (pXYCurve.NumPoints - 1) + 1);
            for k := 0 to pXYCurve.NumPoints - 1 do
                Result[k] := pXYCurve.YValue_pt[k + 1];
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51013);
        end;
    end;

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
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            pXYCurve.X := Value;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51010);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_y(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            pXYCurve.Y := Value;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51010);
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yarray(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    pXYCurve: TXYCurveObj;
    i, k, LoopLimit: Integer;

begin
    Value := PDoubleArray(ValuePtr);
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin

        // Only put in as many points as we have allocated
            LoopLimit := (ValueCount - 1);
            if (LoopLimit - (0) + 1) > pXYCurve.NumPoints then
                LoopLimit := (0) + pXYCurve.NumPoints - 1;

            k := 1;
            for i := (0) to LoopLimit do
            begin
                pXYCurve.YValue_pt[k] := Value[i];
                inc(k);
            end;

        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51016);
        end;
    end;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_Xscale(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.FXscale;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51011);
        end;
    end;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_Xshift(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.FXshift;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51011);
        end;
    end;


end;
//------------------------------------------------------------------------------
function XYCurves_Get_Yscale(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.FYscale;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51011);
        end;
    end;


end;
//------------------------------------------------------------------------------
function XYCurves_Get_Yshift(): Double; CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            Result := pXYCurve.FYshift;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51011);
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xscale(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            pXYCurve.FXScale := Value;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51010);
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Xshift(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            pXYCurve.FXShift := Value;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51010);
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yscale(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            pXYCurve.FYScale := Value;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51010);
        end;
    end;

end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_Yshift(Value: Double); CDECL;
var
    pXYCurve: TXYCurveObj;

begin
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            pXYCurve.FYShift := Value;
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51010);
        end;
    end;

end;
//------------------------------------------------------------------------------
function XYCurves_Get_idx(): Integer; CDECL;
begin
    Result := XYCurveClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure XYCurves_Set_idx(Value: Integer); CDECL;
begin
    if XYCurveClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg('Invalid XYCurve index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
procedure XYCurves_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, XYCurveClass.ElementList, False);
end;

procedure XYCurves_Get_AllNames_GR(); CDECL;
// Same as XYCurves_Get_AllNames but uses global result (GR) pointers
begin
    XYCurves_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
end.
