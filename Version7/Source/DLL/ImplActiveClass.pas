unit ImplActiveClass;

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
    TActiveClass = class(TAutoObject, IActiveClass)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_NumElements: Integer; SAFECALL;
        function Get_ActiveClassName: Widestring; SAFECALL;
        function Get_Count: Integer; SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    DSSObject,
    Variants,
    CktElement;

function TActiveClass.Get_AllNames: Olevariant;
var
    idx: Integer;
    k: Integer;

begin
    if (ActiveCircuit <> NIL) and Assigned(ActiveDSSClass) then
        with ActiveCircuit do
        begin
            Result := VarArrayCreate([0, ActiveDSSClass.ElementCount - 1], varOleStr);
            k := 0;
            idx := ActiveDSSClass.First;
            while idx > 0 do
            begin
                Result[k] := ActiveDSSObject.Name;
                Inc(k);
                idx := ActiveDSSClass.Next;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);

end;

function TActiveClass.Get_First: Integer;

begin

    Result := 0;
    if (ActiveCircuit <> NIL) and Assigned(ActiveDSSClass) then
    begin
        Result := ActiveDSSClass.First;  // sets active objects
    end;

end;

function TActiveClass.Get_Next: Integer;

begin

    Result := 0;
    if (ActiveCircuit <> NIL) and Assigned(ActiveDSSClass) then
    begin
        Result := ActiveDSSClass.Next;  // sets active objects
    end;

end;

function TActiveClass.Get_Name: Widestring;
begin
    if Assigned(ActiveDSSObject) then
        Result := ActiveDSSObject.Name
    else
        Result := '';
end;

procedure TActiveClass.Set_Name(const Value: Widestring);
// set object active by name
var
    pelem: TDSSObject;
begin
    if Assigned(ActiveDSSClass) then
    begin
        pelem := ActiveDSSClass.Find(Value);
        if pelem <> NIL then
        begin
            if pelem is TDSSCktElement then
                ActiveCircuit.ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
            else
                ActiveDSSObject := pelem;
        end;
    end;
end;

function TActiveClass.Get_NumElements: Integer;
begin
    if Assigned(ActiveDSSClass) then
        Result := ActiveDSSCLass.ElementCount
    else
        Result := 0;
end;

function TActiveClass.Get_ActiveClassName: Widestring;
begin
    if Assigned(ActiveDSSClass) then
        Result := ActiveDSSCLass.Name
    else
        Result := '';
end;

function TActiveClass.Get_Count: Integer;
begin
    if Assigned(ActiveDSSClass) then
        Result := ActiveDSSCLass.ElementCount
    else
        Result := 0;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TActiveClass, Class_ActiveClass,
        ciInternal, tmApartment);
end.
