unit ImplVsources;

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
    TVsources = class(TAutoObject, IVsources)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_BasekV: Double; SAFECALL;
        function Get_pu: Double; SAFECALL;
        procedure Set_BasekV(Value: Double); SAFECALL;
        procedure Set_pu(Value: Double); SAFECALL;
        function Get_AngleDeg: Double; SAFECALL;
        function Get_Frequency: Double; SAFECALL;
        function Get_Phases: Integer; SAFECALL;
        procedure Set_AngleDeg(Value: Double); SAFECALL;
        procedure Set_Frequency(Value: Double); SAFECALL;
        procedure Set_Phases(Value: Integer); SAFECALL;

    end;

implementation

uses
    ComServ,
    Vsource,
    Variants,
    PointerList,
    DSSGlobals,
    CktElement;

function TVsources.Get_AllNames: Olevariant;
var
    elem: TVsourceObj;
    pList: TPointerList;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
    begin
        if VsourceClass.ElementList.ListSize > 0 then
        begin
            pList := VsourceClass.ElementList;
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

function TVsources.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := VsourceClass.ElementList.ListSize;
end;

function TVsources.Get_First: Integer;
var
    pElem: TVsourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := VsourceClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := VsourceClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;

function TVsources.Get_Next: Integer;
var
    pElem: TVsourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := VsourceClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := VsourceClass.ElementList.ActiveIndex;
                end
                else
                    pElem := VsourceClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;

function TVsources.Get_Name: Widestring;
var
    elem: TDSSCktElement;
begin
    Result := '';
    elem := ActiveCircuit.ActiveCktElement;
    if elem <> NIL then
        Result := elem.Name;
end;

procedure TVsources.Set_Name(const Value: Widestring);
// Set element active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if VsourceClass.SetActive(Value) then
        begin
            ActiveCircuit.ActiveCktElement := VsourceClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Vsource "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;

function TVsources.Get_BasekV: Double;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.kVBase;
end;

function TVsources.Get_pu: Double;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.perunit;
end;

procedure TVsources.Set_BasekV(Value: Double);
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.kVBase := Value;
end;

procedure TVsources.Set_pu(Value: Double);
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.PerUnit := Value;
end;

function TVsources.Get_AngleDeg: Double;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.angle;

end;

function TVsources.Get_Frequency: Double;
var
    elem: TVsourceObj;
begin
    Result := 0.0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.SrcFrequency;

end;

function TVsources.Get_Phases: Integer;
var
    elem: TVsourceObj;
begin
    Result := 0;
    elem := VsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.NPhases;

end;

procedure TVsources.Set_AngleDeg(Value: Double);
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Angle := Value;
end;

procedure TVsources.Set_Frequency(Value: Double);
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.SrcFrequency := Value;
end;

procedure TVsources.Set_Phases(Value: Integer);
var
    elem: TVsourceObj;
begin
    elem := VsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Nphases := Value;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TVsources, Class_Vsources,
        ciInternal, tmApartment);
end.
