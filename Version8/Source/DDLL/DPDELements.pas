unit DPDELements;

interface

function PDElementsI(mode: Longint; arg: Longint): Longint; CDECL;
function PDElementsF(mode: Longint; arg: Double): Double; CDECL;
function PDElementsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

implementation

uses
    DSSGlobals,
    PDElement,
    PDClass,
    SysUtils,
    Bus;

function PDElementsI(mode: Longint; arg: Longint): Longint; CDECL;

var
    ActivePDElement: TPDElement;

begin
    Result := 0; // Default return value
    case mode of
        0:
        begin  // PDElements.Count
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    Result := PDElements.ListSize;
                end;
        end;
        1:
        begin  // PDElements.First
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
        2:
        begin  // PDElements.Next
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
        3:
        begin  // PDElements.IsShunt
            Result := 0;
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveCktElement is TPDElement then
                    begin
                        ActivePDElement := ActiveCktelement as TPDElement;
                        if ActivePDElement.IsShunt then
                            Result := 1;
                    end;
                end;
        end;
        4:
        begin  // PDElements.NumCustomers
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
        5:
        begin  // PDElements.TotalCustomers
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
        6:
        begin  // PDElements.ParentPDElement
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
        7:
        begin   // PDElements.FromTerminal
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
        8:
        begin  // PDElements.SectionID
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
        end
    else
        Result := -1;
    end;
end;

//**************************Floating point type properties***********************
function PDElementsF(mode: Longint; arg: Double): Double; CDECL;

var
    ActivePDElement: TPDElement;

begin
    Result := 0.0; // Default return value
    case mode of
        0:
        begin  // PDElements.FaultRate read
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
        1:
        begin  // PDElements.FaultRate write
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveCktElement is TPDElement then
                    begin
                        ActivePDElement := ActiveCktelement as TPDElement;
                        ActivePDElement.FaultRate := arg;
                    end;
                end;
        end;
        2:
        begin  // PDElements.PctPermanent read
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
        3:
        begin  // PDElements.PctPermanent write
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveCktElement is TPDElement then
                    begin
                        ActivePDElement := ActiveCktelement as TPDElement;
                        ActivePDElement.PctPerm := arg;
                    end;
                end;
        end;
        4:
        begin  // PDElements.Lambda
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
        5:
        begin  // PDElements.AccumulatedL
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
        6:
        begin  // PDElements.RepairTime
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
        7:
        begin  // PDElements.TotalMiles
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
        end
    else
        Result := -1.0;
    end;
end;

//*************************String type properties*******************************
function PDElementsS(mode: Longint; arg: pAnsiChar): pAnsiChar; CDECL;

var
    ActivePDElement: TPDElement;
    TestString: String;

begin
    Result := pAnsiChar(Ansistring('0')); // Default return value
    case mode of
        0:
        begin  // PDElements.Name read
            Result := '';   // return null if not a PD element
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    if ActiveCktElement is TPDElement then
                    begin
                        ActivePDElement := ActiveCktelement as TPDElement;
                        with ActivePDElement do
                            Result := pAnsiChar(Ansistring(Format('%s.%s', [Parentclass.Name, Name])));  // full name
                    end;
                end;
        end;
        1:
        begin  // PDElements.Name write
            if Assigned(ActiveCircuit[ActiveActor]) then
                with ActiveCircuit[ActiveActor] do
                begin
                    TestString := Widestring(arg);
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
        end
    else
        Result := pAnsiChar(Ansistring('Error, parameter not valid'));
    end;
end;

end.
