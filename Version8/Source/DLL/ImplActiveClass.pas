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
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass) then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := VarArrayCreate([0, ActiveDSSClass[ActiveActor].ElementCount - 1], varOleStr);
            k := 0;
            idx := ActiveDSSClass[ActiveActor].First;
            while idx > 0 do
            begin
                Result[k] := ActiveDSSObject[ActiveActor].Name;
                Inc(k);
                idx := ActiveDSSClass[ActiveActor].Next;
            end;
        end
    else
        Result := VarArrayCreate([0, 0], varOleStr);

end;

function TActiveClass.Get_First: Integer;

begin

    Result := 0;
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
    begin
        Result := ActiveDSSClass[ActiveActor].First;  // sets active objects
    end;

end;

function TActiveClass.Get_Next: Integer;

begin

    Result := 0;
    if (ActiveCircuit[ActiveActor] <> NIL) and Assigned(ActiveDSSClass[ActiveActor]) then
    begin
        Result := ActiveDSSClass[ActiveActor].Next;  // sets active objects
    end;

end;

function TActiveClass.Get_Name: Widestring;
begin
    if Assigned(ActiveDSSObject[ActiveActor]) then
        Result := ActiveDSSObject[ActiveActor].Name
    else
        Result := '';
end;

procedure TActiveClass.Set_Name(const Value: Widestring);
// set object active by name
var
    pelem: TDSSObject;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
    begin
        pelem := ActiveDSSClass[ActiveActor].Find(Value);
        if pelem <> NIL then
        begin
            if pelem is TDSSCktElement then
                ActiveCircuit[ActiveActor].ActiveCktElement := TDSSCktElement(pelem)  // sets ActiveDSSobject
            else
                ActiveDSSObject[ActiveActor] := pelem;
        end;
    end;
end;

function TActiveClass.Get_NumElements: Integer;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
        Result := ActiveDSSCLass[ActiveActor].ElementCount
    else
        Result := 0;
end;

function TActiveClass.Get_ActiveClassName: Widestring;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
        Result := ActiveDSSCLass[ActiveActor].Name
    else
        Result := '';
end;

function TActiveClass.Get_Count: Integer;
begin
    if Assigned(ActiveDSSClass[ActiveActor]) then
        Result := ActiveDSSCLass[ActiveActor].ElementCount
    else
        Result := 0;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TActiveClass, Class_ActiveClass,
        ciInternal, tmApartment);
end.
