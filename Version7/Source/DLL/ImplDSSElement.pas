unit ImplDSSElement;

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
    TDSSElement = class(TAutoObject, IDSSElement)
    PROTECTED
        function Get_AllPropertyNames: Olevariant; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_NumProperties: Integer; SAFECALL;
        function Get_Properties(Indx: Olevariant): IDSSProperty; SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Variants,
    ImplGlobals,
    Sysutils;

function TDSSElement.Get_AllPropertyNames: Olevariant;
var
    k: Integer;
begin
    Result := VarArrayCreate([0, 0], varOleStr);
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveDSSObject <> NIL then
                with ActiveDSSObject do
                begin
                    with ParentClass do
                    begin
                        Result := VarArrayCreate([0, NumProperties - 1], varOleStr);
                        for k := 1 to NumProperties do
                        begin
                            Result[k - 1] := PropertyName^[k];
                        end;
                    end;
                end
        end;

end;

function TDSSElement.Get_Name: Widestring;
begin
    if ActiveCircuit <> NIL then
        if ActiveDSSObject <> NIL then
            with ActiveDSSObject do
            begin
                Result := ParentClass.Name + '.' + Name;
            end
        else
            Result := '';
end;

function TDSSElement.Get_NumProperties: Integer;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
        begin
            if ActiveDSSObject <> NIL then
                with ActiveDSSObject do
                begin
                    Result := ParentClass.NumProperties;
                end
        end;
end;

function TDSSElement.Get_Properties(Indx: Olevariant): IDSSProperty;
var
    Str: String;
    i: Integer;
begin

    if ActiveCircuit <> NIL then
    begin

        case (Vartype(Indx) and VarTypeMask) of
            VarSmallint, VarInteger:
                FPropIndex := Integer(Indx) + 1;    // INdex is zero based to match arrays
            VarOleStr:
            begin
                FPropClass := ActiveDSSObject.ParentClass;
                FPropIndex := 0;
                Str := Indx;
                if FPropClass <> NIL then
                    with FPropClass do
                        for i := 1 to NumProperties do
                        begin
                            if CompareText(Str, PropertyName^[i]) = 0 then
                            begin
                                FPropIndex := i;
                                Break;
                            end;
                        end;
            end;
        else
            DoSimpleMsg('Illegal Var Type Passed to Properties Interface: ' + Format('$%x', [VarType(Indx)]), 5011);
        end;

    end;

    Result := FDSSProperty as IDSSProperty;

end;

initialization
    TAutoObjectFactory.Create(ComServer, TDSSElement, Class_DSSElement,
        ciInternal, tmApartment);
end.
