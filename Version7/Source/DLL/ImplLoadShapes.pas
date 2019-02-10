unit ImplLoadShapes;

 {
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TLoadShapes = class(TAutoObject, ILoadShapes)
    PROTECTED
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Npts: Integer; SAFECALL;
        function Get_Pmult: Olevariant; SAFECALL;
        function Get_Qmult: Olevariant; SAFECALL;
        procedure Set_Npts(Value: Integer); SAFECALL;
        procedure Set_Pmult(Value: Olevariant); SAFECALL;
        procedure Set_Qmult(Value: Olevariant); SAFECALL;
        procedure Normalize; SAFECALL;
        function Get_TimeArray: Olevariant; SAFECALL;
        procedure Set_TimeArray(Value: Olevariant); SAFECALL;
        function Get_HrInterval: Double; SAFECALL;
        function Get_MinInterval: Double; SAFECALL;
        function Get_sInterval: Double; SAFECALL;
        procedure Set_HrInterval(Value: Double); SAFECALL;
        procedure Set_MinInterval(Value: Double); SAFECALL;
        procedure Set_Sinterval(Value: Double); SAFECALL;
        function New(const Name: Widestring): Integer; STDCALL;
        function Get_PBase: Double; SAFECALL;
        function Get_Qbase: Double; SAFECALL;
        procedure Set_PBase(Value: Double); SAFECALL;
        procedure Set_Qbase(Value: Double); SAFECALL;
        function Get_UseActual: Wordbool; SAFECALL;
        procedure Set_UseActual(Value: Wordbool); SAFECALL;

    end;

implementation

{
  In this implementation, operate on ActiveLSObject instead of activeDSSobject
}

uses
    ComServ,
    Loadshape,
    DSSGlobals,
    PointerList,
    Variants,
    ExecHelper;

var
    ActiveLSObject: TLoadshapeObj;

function TLoadShapes.Get_Name: Widestring;
var
    elem: TLoadshapeObj;
begin
    Result := '';
    elem := LoadshapeClass.GetActiveObj;
    if elem <> NIL then
        Result := elem.Name;

end;

procedure TLoadShapes.Set_Name(const Value: Widestring);
// Set element active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if LoadshapeClass.SetActive(Value) then
        begin
            ActiveLSObject := LoadshapeClass.ElementList.Active;
            ActiveDSSObject := ActiveLSObject;
        end
        else
        begin
            DoSimpleMsg('Relay "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;

end;

function TLoadShapes.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := LoadshapeClass.ElementList.ListSize;
end;

function TLoadShapes.Get_First: Integer;
var
    iElem: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        iElem := LoadshapeClass.First;
        if iElem <> 0 then
        begin
            ActiveLSObject := ActiveDSSObject as TLoadShapeObj;
            Result := 1;
        end
    end;
end;

function TLoadShapes.Get_Next: Integer;
var
    iElem: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        iElem := LoadshapeClass.Next;
        if iElem <> 0 then
        begin
            ActiveLSObject := ActiveDSSObject as TLoadShapeObj;
            Result := iElem;
        end
    end;
end;

function TLoadShapes.Get_AllNames: Olevariant;
var
    elem: TLoadshapeObj;
    pList: TPointerList;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
    begin
        if LoadShapeClass.ElementList.ListSize > 0 then
        begin
            pList := LoadShapeClass.ElementList;
            VarArrayRedim(Result, pList.ListSize - 1);
            k := 0;
            elem := pList.First;
            while elem <> NIL do
            begin
                Result[k] := elem.Name;
                Inc(k);
                elem := pList.next;
            end;
        end;
    end;

end;

function TLoadShapes.Get_Npts: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            Result := ActiveLSObject.NumPoints;
end;

function TLoadShapes.Get_Pmult: Olevariant;
var
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
        begin
            VarArrayRedim(Result, ActiveLSObject.NumPoints - 1);
            for k := 0 to ActiveLSObject.NumPoints - 1 do
                Result[k] := ActiveLSObject.PMultipliers^[k + 1];
        end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61001);
        end;
    end;
end;

function TLoadShapes.Get_Qmult: Olevariant;
var
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
        begin
            if assigned(ActiveLSObject.QMultipliers) then
            begin
                VarArrayRedim(Result, ActiveLSObject.NumPoints - 1);
                for k := 0 to ActiveLSObject.NumPoints - 1 do
                    Result[k] := ActiveLSObject.QMultipliers^[k + 1];
            end;
        end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61001);
        end;
    end;
end;

procedure TLoadShapes.Set_Npts(Value: Integer);
begin
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.NumPoints := Value;
end;

procedure TLoadShapes.Set_Pmult(Value: Olevariant);
var
    i, k, LoopLimit: Integer;

begin
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
            with ActiveLSObject do
            begin

        // Only put in as many points as we have allocated
                LoopLimit := VarArrayHighBound(Value, 1);
                if (LoopLimit - VarArrayLowBound(Value, 1) + 1) > NumPoints then
                    LoopLimit := VarArrayLowBound(Value, 1) + NumPoints - 1;

                ReallocMem(PMultipliers, Sizeof(PMultipliers^[1]) * NumPoints);
                k := 1;
                for i := VarArrayLowBound(Value, 1) to LoopLimit do
                begin
                    ActiveLSObject.Pmultipliers^[k] := Value[i];
                    inc(k);
                end;

            end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61002);
        end;
    end;
end;

procedure TLoadShapes.Set_Qmult(Value: Olevariant);
var
    i, k, LoopLimit: Integer;

begin
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
            with ActiveLSObject do
            begin

        // Only put in as many points as we have allocated
                LoopLimit := VarArrayHighBound(Value, 1);
                if (LoopLimit - VarArrayLowBound(Value, 1) + 1) > NumPoints then
                    LoopLimit := VarArrayLowBound(Value, 1) + NumPoints - 1;

                ReallocMem(QMultipliers, Sizeof(QMultipliers^[1]) * NumPoints);
                k := 1;
                for i := VarArrayLowBound(Value, 1) to LoopLimit do
                begin
                    ActiveLSObject.Qmultipliers^[k] := Value[i];
                    inc(k);
                end;

            end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61002);
        end;
    end;
end;

procedure TLoadShapes.Normalize;
begin

    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.Normalize;
end;

function TLoadShapes.Get_TimeArray: Olevariant;
var
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    Result[0] := 0.0;  // error condition: one element array=0
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
        begin
            if ActiveLSObject.hours <> NIL then
            begin
                VarArrayRedim(Result, ActiveLSObject.NumPoints - 1);
                for k := 0 to ActiveLSObject.NumPoints - 1 do
                    Result[k] := ActiveLSObject.Hours^[k + 1];
            end
        end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61001);
        end;
    end;
end;

procedure TLoadShapes.Set_TimeArray(Value: Olevariant);
var
    i, k, LoopLimit: Integer;

begin
    if ActiveCircuit <> NIL then
    begin
        if ActiveLSObject <> NIL then
            with ActiveLSObject do
            begin

        // Only put in as many points as we have allocated
                LoopLimit := VarArrayHighBound(Value, 1);
                if (LoopLimit - VarArrayLowBound(Value, 1) + 1) > NumPoints then
                    LoopLimit := VarArrayLowBound(Value, 1) + NumPoints - 1;

                ReallocMem(Hours, Sizeof(Hours^[1]) * NumPoints);
                k := 1;
                for i := VarArrayLowBound(Value, 1) to LoopLimit do
                begin
                    ActiveLSObject.Hours^[k] := Value[i];
                    inc(k);
                end;

            end
        else
        begin
            DoSimpleMsg('No active Loadshape Object found.', 61002);
        end;
    end;

end;

function TLoadShapes.Get_HrInterval: Double;

begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            Result := ActiveLSObject.Interval;

end;

function TLoadShapes.Get_MinInterval: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            Result := ActiveLSObject.Interval * 60.0;
end;

function TLoadShapes.Get_sInterval: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            Result := ActiveLSObject.Interval * 3600.0;
end;

procedure TLoadShapes.Set_HrInterval(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.Interval := Value;
end;

procedure TLoadShapes.Set_MinInterval(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.Interval := Value / 60.0;
end;

procedure TLoadShapes.Set_Sinterval(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.Interval := Value / 3600.0;
end;

function TLoadShapes.New(const Name: Widestring): Integer;
begin
    Result := AddObject('loadshape', Name);    // Returns handle to object
    ActiveLSObject := ActiveDSSObject as TLoadShapeObj;
end;

function TLoadShapes.Get_PBase: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            Result := ActiveLSObject.baseP;
end;

function TLoadShapes.Get_Qbase: Double;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            Result := ActiveLSObject.baseQ;
end;

procedure TLoadShapes.Set_PBase(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.baseP := Value;
end;

procedure TLoadShapes.Set_Qbase(Value: Double);
begin
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.baseQ := Value;
end;

function TLoadShapes.Get_UseActual: Wordbool;
begin
    Result := FALSE;
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            Result := ActiveLSObject.UseActual;
end;

procedure TLoadShapes.Set_UseActual(Value: Wordbool);
begin
    if ActiveCircuit <> NIL then
        if ActiveLSObject <> NIL then
            ActiveLSObject.UseActual := Value;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TLoadShapes, Class_LoadShapes,
        ciInternal, tmApartment);
end.
