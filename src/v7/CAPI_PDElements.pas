unit CAPI_PDElements;

{$inline on}

interface

uses
    CAPI_Utils;

function PDElements_Get_Count(): Integer; CDECL;
function PDElements_Get_FaultRate(): Double; CDECL;
function PDElements_Get_First(): Integer; CDECL;
function PDElements_Get_IsShunt(): Boolean; CDECL;
function PDElements_Get_Next(): Integer; CDECL;
function PDElements_Get_pctPermanent(): Double; CDECL;
procedure PDElements_Set_FaultRate(Value: Double); CDECL;
procedure PDElements_Set_pctPermanent(Value: Double); CDECL;
function PDElements_Get_Name(): PAnsiChar; CDECL;
procedure PDElements_Set_Name(const Value: PAnsiChar); CDECL;
function PDElements_Get_AccumulatedL(): Double; CDECL;
function PDElements_Get_Lambda(): Double; CDECL;
function PDElements_Get_Numcustomers(): Integer; CDECL;
function PDElements_Get_ParentPDElement(): Integer; CDECL;
function PDElements_Get_RepairTime(): Double; CDECL;
function PDElements_Get_Totalcustomers(): Integer; CDECL;
function PDElements_Get_FromTerminal(): Integer; CDECL;
function PDElements_Get_TotalMiles(): Double; CDECL;
function PDElements_Get_SectionID(): Integer; CDECL;
procedure PDElements_Set_RepairTime(Value: Double); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    PDElement,
    PDClass,
    SysUtils,
    Bus;

function PDElements_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            Result := PDElements.ListSize;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FaultRate(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.Faultrate;
            end;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_First(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
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
//------------------------------------------------------------------------------
function PDElements_Get_IsShunt(): Boolean; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := FALSE;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.IsShunt;
            end;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Next(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
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
//------------------------------------------------------------------------------
function PDElements_Get_pctPermanent(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.PctPerm;
            end;
        end;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_FaultRate(Value: Double); CDECL;
var
    ActivePDElement: TPDElement;
begin
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                ActivePDElement.FaultRate := Value;
            end;
        end;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_pctPermanent(Value: Double); CDECL;
var
    ActivePDElement: TPDElement;
begin
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                ActivePDElement.PctPerm := Value;
            end;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Name_AnsiString(): Ansistring; inline;
var
    ActivePDElement: TPDElement;
begin
    Result := '';   // return null if not a PD element
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                with ActivePDElement do
                    Result := Format('%s.%s', [Parentclass.Name, Name]);  // full name
            end;
        end;
end;

function PDElements_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(PDElements_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_Name(const Value: PAnsiChar); CDECL;
var
    ActivePDElement: TPDElement;
    TestString: String;

begin
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
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
//------------------------------------------------------------------------------
function PDElements_Get_AccumulatedL(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.AccumulatedBrFltRate;
            end;
        end;

end;
//------------------------------------------------------------------------------
function PDElements_Get_Lambda(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchFltRate;
            end;
        end;

end;
//------------------------------------------------------------------------------
function PDElements_Get_Numcustomers(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchNumCustomers;
            end;
        end;

end;
//------------------------------------------------------------------------------
function PDElements_Get_ParentPDElement(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
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
//------------------------------------------------------------------------------
function PDElements_Get_RepairTime(): Double; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.HrsToRepair;
            end;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Totalcustomers(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchTotalCustomers;
            end;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FromTerminal(): Integer; CDECL;
var
    ActivePDElement: TPDElement;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.FromTerminal;
            end;
        end;

end;
//------------------------------------------------------------------------------
function PDElements_Get_TotalMiles(): Double; CDECL;
// Total miles of line from here on down to the end of the feeder

var
    ActivePDElement: TPDElement;

begin
    Result := 0.0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.AccumulatedMilesDownStream;
            end;
        end;
end;
//------------------------------------------------------------------------------
function PDElements_Get_SectionID(): Integer; CDECL;
var
    ActivePDElement: TPDElement;

begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                Result := ActivePDElement.BranchSectionID;
            end;
        end;

end;
//------------------------------------------------------------------------------
procedure PDElements_Set_RepairTime(Value: Double); CDECL;
var
    ActivePDElement: TPDElement;
begin
    if Assigned(ActiveCircuit) then
        with ActiveCircuit do
        begin
            if ActiveCktElement is TPDElement then
            begin
                ActivePDElement := ActiveCktelement as TPDElement;
                ActivePDElement.HrsToRepair := Value;
            end;
        end;
end;
//------------------------------------------------------------------------------
end.
