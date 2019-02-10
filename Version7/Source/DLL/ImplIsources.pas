unit ImplIsources;

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
    TISources = class(TAutoObject, IISources)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_Amps: Double; SAFECALL;
        procedure Set_Amps(Value: Double); SAFECALL;
        function Get_AngleDeg: Double; SAFECALL;
        function Get_Frequency: Double; SAFECALL;
        procedure Set_AngleDeg(Value: Double); SAFECALL;
        procedure Set_Frequency(Value: Double); SAFECALL;

    end;

implementation

uses
    ComServ,
    Variants,
    PointerList,
    Isource,
    DSSGlobals,
    CktElement;

function TISources.Get_AllNames: Olevariant;
var
    elem: TIsourceObj;
    pList: TPointerList;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
    begin
        if IsourceClass.ElementList.ListSize > 0 then
        begin
            pList := IsourceClass.ElementList;
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

function TISources.Get_Count: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := IsourceClass.ElementList.ListSize;
end;

function TISources.Get_First: Integer;
var
    pElem: TIsourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := IsourceClass.ElementList.First;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := 1;
                end
                else
                    pElem := IsourceClass.ElementList.Next;
            until (Result = 1) or (pElem = NIL);
    end;
end;

function TISources.Get_Next: Integer;
var
    pElem: TIsourceObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pElem := IsourceClass.ElementList.Next;
        if pElem <> NIL then
            repeat
                if pElem.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pElem;
                    Result := IsourceClass.ElementList.ActiveIndex;
                end
                else
                    pElem := IsourceClass.ElementList.Next;
            until (Result > 0) or (pElem = NIL);
    end;
end;

function TISources.Get_Name: Widestring;
var
    elem: TDSSCktElement;
begin
    Result := '';
    elem := ActiveCircuit.ActiveCktElement;
    if elem <> NIL then
        Result := elem.Name;
end;

procedure TISources.Set_Name(const Value: Widestring);
// Set element active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if IsourceClass.SetActive(Value) then
        begin
            ActiveCircuit.ActiveCktElement := IsourceClass.ElementList.Active;
        end
        else
        begin
            DoSimpleMsg('Isource "' + Value + '" Not Found in Active Circuit.', 77003);
        end;
    end;
end;

function TISources.Get_Amps: Double;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := IsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Amps;
end;

procedure TISources.Set_Amps(Value: Double);
var
    elem: TIsourceObj;
begin
    elem := IsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Amps := Value;
end;

function TISources.Get_AngleDeg: Double;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := IsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.Angle;
end;

function TISources.Get_Frequency: Double;
var
    elem: TIsourceObj;
begin
    Result := 0.0;
    elem := IsourceClass.ElementList.Active;
    if elem <> NIL then
        Result := elem.SrcFrequency;
end;

procedure TISources.Set_AngleDeg(Value: Double);
var
    elem: TIsourceObj;
begin
    elem := IsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.Angle := Value;
end;

procedure TISources.Set_Frequency(Value: Double);
var
    elem: TIsourceObj;
begin
    elem := IsourceClass.GetActiveObj;
    if elem <> NIL then
        elem.SrcFrequency := Value;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TISources, Class_ISources,
        ciInternal, tmApartment);
end.
