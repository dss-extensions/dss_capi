unit ImplPDElements;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{$WARN SYMBOL_PLATFORM OFF}

{Created 8/19/13}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TPDElements = class(TAutoObject, IPDElements)
    PROTECTED
        function Get_Count: Integer; SAFECALL;
        function Get_FaultRate: Double; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_IsShunt: Wordbool; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        function Get_pctPermanent: Double; SAFECALL;
        procedure Set_FaultRate(Value: Double); SAFECALL;
        procedure Set_pctPermanent(Value: Double); SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_AccumulatedL: Double; SAFECALL;
        function Get_Lambda: Double; SAFECALL;
        function Get_Numcustomers: Integer; SAFECALL;
        function Get_ParentPDElement: Integer; SAFECALL;
        function Get_RepairTime: Double; SAFECALL;
        function Get_Totalcustomers: Integer; SAFECALL;
        function Get_FromTerminal: Integer; SAFECALL;
        function Get_TotalMiles: Double; SAFECALL;
        function Get_SectionID: Integer; SAFECALL;
        procedure Set_RepairTime(Value: Double); SAFECALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    PDElement,
    PDClass,
    SysUtils,
    Bus;

function TPDElements.Get_Count: Integer;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            Result := PDElements.ListSize;
        end;
end;

function TPDElements.Get_FaultRate: Double;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.Faultrate;
            end;
        end;
end;

function TPDElements.Get_First: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            ActivePDElement := PDElements.First;
            if ActivePDElement <> NIL then
            begin
                repeat
                    if ActivePDElement.enabled then
                    begin
                        Result := 1;
                        ActiveCktElement := ActivePDElement;
                    end
                    else
                        ActivePDElement := PDElements.Next;
                until (Result = 1) or (ActivePDELement = NIL);
            end;
        end;
end;

function TPDElements.Get_IsShunt: Wordbool;
var
    ActivePDElement: TPDElement;
begin
    Result := FALSE;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.IsShunt;
            end;
        end;
end;

function TPDElements.Get_Next: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            ActivePDElement := PDElements.Next;
            if ActivePDElement <> NIL then
            begin
                repeat
                    if ActivePDElement.enabled then
                    begin
                        Result := 1;
                        ActiveCktElement := ActivePDElement;
                    end
                    else
                        ActivePDElement := PDElements.Next;
                until (Result = 1) or (ActivePDELement = NIL);
            end;
        end;
end;

function TPDElements.Get_pctPermanent: Double;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.PctPerm;
            end;
        end;
end;

procedure TPDElements.Set_FaultRate(Value: Double);
var
    ActivePDElement: TPDElement;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                ActivePDElement.FaultRate := Value;
            end;
        end;
end;

procedure TPDElements.Set_pctPermanent(Value: Double);
var
    ActivePDElement: TPDElement;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                ActivePDElement.PctPerm := Value;
            end;
        end;
end;

function TPDElements.Get_Name: Widestring;

var
    ActivePDElement: TPDElement;
begin
    Result := '';   // return null if not a PD element
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                with ActivePDElement do
                    Result := Format('%s.%s', [Parentclass.Name, Name]);  // full name
            end;
        end;
end;

procedure TPDElements.Set_Name(const Value: Widestring);
var
    ActivePDElement: TPDElement;
    TestString: String;

begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            TestString := Value;
          // Search through list of PD Elements until we find this one
            ActivePDElement := PDElements.First;
            while Assigned(ActivePDElement) do
                with ActivePDelement do
                begin
                    if (CompareText(TestString, Format('%s.%s', [Parentclass.Name, Name])) = 0) then
                    begin
                        ActiveCktElement := ActivePDElement;
                        Break;
                    end;
                    ActivePDElement := PDElements.Next;
                end;
        end;
end;

function TPDElements.Get_AccumulatedL: Double;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.AccumulatedBrFltRate;
            end;
        end;

end;

function TPDElements.Get_Lambda: Double;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchFltRate;
            end;
        end;

end;

function TPDElements.Get_Numcustomers: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchNumCustomers;
            end;
        end;

end;

function TPDElements.Get_ParentPDElement: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                if ActivePDElement.ParentPDElement <> NIL then    // leaves ActiveCktElement as is
                begin
                    ActiveCktElement := ActivePDElement.ParentPDElement;
                    Result := ActivecktElement.ClassIndex;
                end;
            end;
        end;
end;

function TPDElements.Get_RepairTime: Double;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.HrsToRepair;
            end;
        end;
end;

function TPDElements.Get_Totalcustomers: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchTotalCustomers;
            end;
        end;
end;

function TPDElements.Get_FromTerminal: Integer;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.FromTerminal;
            end;
        end;

end;

function TPDElements.Get_TotalMiles: Double;
// Total miles of line from here on down to the end of the feeder

var
    ActivePDElement: TPDElement;

begin
    Result := 0.0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.AccumulatedMilesDownStream;
            end;
        end;
end;

function TPDElements.Get_SectionID: Integer;
var
    ActivePDElement: TPDElement;

begin
    Result := 0;
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchSectionID;
            end;
        end;

end;

procedure TPDElements.Set_RepairTime(Value: Double);
var
    ActivePDElement: TPDElement;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        with ActiveCircuit[ActiveActor] do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                ActivePDElement.HrsToRepair := Value;
            end;
        end;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TPDElements, Class_PDElements,
        ciInternal, tmApartment);
end.
