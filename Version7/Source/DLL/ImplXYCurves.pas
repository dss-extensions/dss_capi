unit ImplXYCurves;

{$WARN SYMBOL_PLATFORM OFF}
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl,
    XYCurve,
    DSSClass;

type
    TXYCurves = class(TAutoObject, IXYCurves)
    PROTECTED
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Npts: Integer; SAFECALL;
        function Get_Xarray: Olevariant; SAFECALL;
        procedure Set_Npts(Value: Integer); SAFECALL;
        procedure Set_Xarray(Value: Olevariant); SAFECALL;
        function Get_x: Double; SAFECALL;
        function Get_y: Double; SAFECALL;
        function Get_Yarray: Olevariant; SAFECALL;
        procedure Set_x(Value: Double); SAFECALL;
        procedure Set_y(Value: Double); SAFECALL;
        procedure Set_Yarray(Value: Olevariant); STDCALL;
        function Get_Xscale: Double; SAFECALL;
        function Get_Xshift: Double; SAFECALL;
        function Get_Yscale: Double; SAFECALL;
        function Get_Yshift: Double; SAFECALL;
        procedure Set_Xscale(Value: Double); SAFECALL;
        procedure Set_Xshift(Value: Double); SAFECALL;
        procedure Set_Yscale(Value: Double); SAFECALL;
        procedure Set_Yshift(Value: Double); SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    DSSObject,
    Variants;

function TXYCurves.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := XYCurveClass.ElementCount;
end;

function TXYCurves.Get_First: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := XYCurveClass.First;
end;

function TXYCurves.Get_Name: Widestring;

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

function TXYCurves.Get_Next: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := XYCurveClass.Next;
end;

procedure TXYCurves.Set_Name(const Value: Widestring);

// set XYCurve active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if not XYCurveClass.SetActive(Value) then
            DoSimpleMsg('XYCurve "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;

function TXYCurves.Get_Npts: Integer;

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

function TXYCurves.Get_Xarray: Olevariant;
var
    pXYCurve: TXYCurveObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            VarArrayRedim(Result, pXYCurve.NumPoints - 1);
            for k := 0 to pXYCurve.NumPoints - 1 do
                Result[k] := pXYCurve.XValue_pt[k + 1];
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51013);
        end;
    end;
end;

procedure TXYCurves.Set_Npts(Value: Integer);
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

procedure TXYCurves.Set_Xarray(Value: Olevariant);
var
    pXYCurve: TXYCurveObj;
    i, k, LoopLimit: Integer;

begin
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin

        // Only put in as many points as we have allocated
            LoopLimit := VarArrayHighBound(Value, 1);
            if (LoopLimit - VarArrayLowBound(Value, 1) + 1) > pXYCurve.NumPoints then
                LoopLimit := VarArrayLowBound(Value, 1) + pXYCurve.NumPoints - 1;

            k := 1;
            for i := VarArrayLowBound(Value, 1) to LoopLimit do
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

function TXYCurves.Get_x: Double;
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

function TXYCurves.Get_y: Double;
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

function TXYCurves.Get_Yarray: Olevariant;
var
    pXYCurve: TXYCurveObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin
            VarArrayRedim(Result, pXYCurve.NumPoints - 1);
            for k := 0 to pXYCurve.NumPoints - 1 do
                Result[k] := pXYCurve.YValue_pt[k + 1];
        end
        else
        begin
            DoSimpleMsg('No active XYCurve Object found.', 51013);
        end;
    end;

end;

procedure TXYCurves.Set_x(Value: Double);
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

procedure TXYCurves.Set_y(Value: Double);
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

procedure TXYCurves.Set_Yarray(Value: Olevariant);
var
    pXYCurve: TXYCurveObj;
    i, k, LoopLimit: Integer;

begin
    if ActiveCircuit <> NIL then
    begin
        pXYCurve := XYCurveClass.GetActiveObj;
        if pXYCurve <> NIL then
        begin

        // Only put in as many points as we have allocated
            LoopLimit := VarArrayHighBound(Value, 1);
            if (LoopLimit - VarArrayLowBound(Value, 1) + 1) > pXYCurve.NumPoints then
                LoopLimit := VarArrayLowBound(Value, 1) + pXYCurve.NumPoints - 1;

            k := 1;
            for i := VarArrayLowBound(Value, 1) to LoopLimit do
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

function TXYCurves.Get_Xscale: Double;
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

function TXYCurves.Get_Xshift: Double;
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

function TXYCurves.Get_Yscale: Double;
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

function TXYCurves.Get_Yshift: Double;
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

procedure TXYCurves.Set_Xscale(Value: Double);
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

procedure TXYCurves.Set_Xshift(Value: Double);
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

procedure TXYCurves.Set_Yscale(Value: Double);
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

procedure TXYCurves.Set_Yshift(Value: Double);
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

initialization
    TAutoObjectFactory.Create(ComServer, TXYCurves, Class_XYCurves,
        ciInternal, tmApartment);
end.
