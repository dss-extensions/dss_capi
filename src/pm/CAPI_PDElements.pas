UNIT CAPI_PDElements;
{$inline on}

INTERFACE

USES CAPI_Utils;

function PDElements_Get_Count():Integer;cdecl;
function PDElements_Get_FaultRate():Double;cdecl;
function PDElements_Get_First():Integer;cdecl;
function PDElements_Get_IsShunt():WordBool;cdecl;
function PDElements_Get_Next():Integer;cdecl;
function PDElements_Get_pctPermanent():Double;cdecl;
procedure PDElements_Set_FaultRate(Value: Double);cdecl;
procedure PDElements_Set_pctPermanent(Value: Double);cdecl;
function PDElements_Get_Name():PAnsiChar;cdecl;
procedure PDElements_Set_Name(const Value: PAnsiChar);cdecl;
function PDElements_Get_AccumulatedL():Double;cdecl;
function PDElements_Get_Lambda():Double;cdecl;
function PDElements_Get_Numcustomers():Integer;cdecl;
function PDElements_Get_ParentPDElement():Integer;cdecl;
function PDElements_Get_RepairTime():Double;cdecl;
function PDElements_Get_Totalcustomers():Integer;cdecl;
function PDElements_Get_FromTerminal():Integer;cdecl;
function PDElements_Get_TotalMiles():Double;cdecl;
function PDElements_Get_SectionID():Integer;cdecl;
procedure PDElements_Set_RepairTime(Value: Double);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, PDElement, PDClass, SysUtils, Bus;

function PDElements_Get_Count():Integer;cdecl;
begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          Result := PDElements.ListSize ;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FaultRate():Double;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.Faultrate;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_First():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
           ActivePDElement := PDElements.First;
           IF ActivePDElement <> Nil THEN
             Begin
                  Repeat
                    If ActivePDElement.enabled  Then
                    Begin
                        Result := 1;
                        ActiveCktElement := ActivePDElement;
                    end
                    Else  ActivePDElement := PDElements.Next;
                  Until (Result = 1) or (ActivePDELement = nil);
             End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_IsShunt():WordBool;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := FALSE;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.IsShunt;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Next():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
           ActivePDElement := PDElements.Next;
           IF ActivePDElement <> Nil THEN
             Begin
                  Repeat
                    If ActivePDElement.enabled  Then
                    Begin
                        Result := 1;
                        ActiveCktElement := ActivePDElement;
                    end
                    Else  ActivePDElement := PDElements.Next;
                  Until (Result = 1) or (ActivePDELement = nil);
             End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_pctPermanent():Double;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.PctPerm;
          End;
      End;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_FaultRate(Value: Double);cdecl;
Var
   ActivePDElement :TPDElement;
begin
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.FaultRate := Value;
          End;
      End;
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_pctPermanent(Value: Double);cdecl;
Var
   ActivePDElement :TPDElement;
begin
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.PctPerm := Value;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Name_AnsiString():AnsiString;inline;
Var
   ActivePDElement :TPDElement;
begin
     Result := '';   // return null if not a PD element
      If Assigned(ActiveCircuit[ActiveActor]) Then
        With ActiveCircuit[ActiveActor] Do Begin
            If ActiveCktElement is TPDElement Then Begin
                ActivePDElement := ActiveCktelement as TPDElement;
                With ActivePDElement Do
                     Result := Format('%s.%s',[Parentclass.Name, Name]);  // full name
            End;
        End;
end;

function PDElements_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(PDElements_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure PDElements_Set_Name(const Value: PAnsiChar);cdecl;
Var
   ActivePDElement :TPDElement;
   TestString : String;

begin
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          TestString := Value;
          // Search through list of PD Elements until we find this one
          ActivePDElement := PDElements.First;
          While Assigned(ActivePDElement) do
          With ActivePDelement Do
          Begin
              If (CompareText(TestString, Format('%s.%s',[Parentclass.Name, Name]) ) = 0)  Then Begin
                 ActiveCktElement := ActivePDElement;
                 Break;
              End;
              ActivePDElement := PDElements.Next;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_AccumulatedL():Double;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.AccumulatedBrFltRate ;
          End;
      End;

end;
//------------------------------------------------------------------------------
function PDElements_Get_Lambda():Double;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchFltRate;
          End;
      End;

end;
//------------------------------------------------------------------------------
function PDElements_Get_Numcustomers():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchNumCustomers;
          End;
      End;

end;
//------------------------------------------------------------------------------
function PDElements_Get_ParentPDElement():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              If ActivePDElement.ParentPDElement <> Nil Then    // leaves ActiveCktElement as is
              Begin
                  ActiveCktElement := ActivePDElement.ParentPDElement;
                  Result := ActivecktElement.ClassIndex ;
              End;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_RepairTime():Double;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.HrsToRepair;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_Totalcustomers():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchTotalCustomers;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_FromTerminal():Integer;cdecl;
Var
   ActivePDElement :TPDElement;
begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.FromTerminal ;
          End;
      End;

end;
//------------------------------------------------------------------------------
function PDElements_Get_TotalMiles():Double;cdecl;
// Total miles of line from here on down to the end of the feeder

Var
   ActivePDElement : TPDElement;

begin
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.AccumulatedMilesDownStream;
          End;
      End;
end;
//------------------------------------------------------------------------------
function PDElements_Get_SectionID():Integer;cdecl;
Var
   ActivePDElement : TPDElement;

begin
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchSectionID ;
          End;
      End;

end;
//------------------------------------------------------------------------------
procedure PDElements_Set_RepairTime(Value: Double);cdecl;
Var
   ActivePDElement :TPDElement;
begin
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.HrsToRepair := Value;
          End;
      End;
end;
//------------------------------------------------------------------------------
END.
