unit DPDELements;

interface

function PDElementsI(mode:longint; arg:longint):longint;cdecl;
function PDElementsF(mode:longint; arg:double):double;cdecl;
function PDElementsS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;

implementation

uses DSSGlobals, PDElement, PDClass, SysUtils, Bus;

function PDElementsI(mode:longint; arg:longint):longint;cdecl;

Var
   ActivePDElement :TPDElement;

begin
  Result:=0; // Default return value
  case mode of
  0: begin  // PDElements.Count
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          Result := PDElements.ListSize ;
      End;
  end;
  1: begin  // PDElements.First
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
  2: begin  // PDElements.Next
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
  3: begin  // PDElements.IsShunt
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              if ActivePDElement.IsShunt then Result:=1;
          End;
      End;
  end;
  4: begin  // PDElements.NumCustomers
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchNumCustomers;
          End;
      End;
  end;
  5: begin  // PDElements.TotalCustomers
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchTotalCustomers;
          End;
      End;
  end;
  6: begin  // PDElements.ParentPDElement
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
  7: begin   // PDElements.FromTerminal
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.FromTerminal ;
          End;
      End;
  end;
  8: begin  // PDElements.SectionID
      Result := 0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchSectionID ;
          End;
      End;
  end
  else
      Result:=-1;
  end;
end;

//**************************Floating point type properties***********************
function PDElementsF(mode:longint; arg:double):double;cdecl;

Var
   ActivePDElement :TPDElement;

begin
  Result:=0.0; // Default return value
  case mode of
  0: begin  // PDElements.FaultRate read
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.Faultrate;
          End;
      End;
  end;
  1: begin  // PDElements.FaultRate write
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.FaultRate := arg;
          End;
      End;
  end;
  2: begin  // PDElements.PctPermanent read
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.PctPerm;
          End;
      End;
  end;
  3: begin  // PDElements.PctPermanent write
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              ActivePDElement.PctPerm := arg;
          End;
      End;
  end;
  4: begin  // PDElements.Lambda
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.BranchFltRate;
          End;
      End;
  end;
  5: begin  // PDElements.AccumulatedL
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.AccumulatedBrFltRate ;
          End;
      End;
  end;
  6: begin  // PDElements.RepairTime
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.HrsToRepair;
          End;
      End;
  end;
  7: begin  // PDElements.TotalMiles
      Result := 0.0;
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          If ActiveCktElement is TPDElement Then Begin
              ActivePDElement := ActiveCktelement as TPDElement;
              Result := ActivePDElement.AccumulatedMilesDownStream;
          End;
      End;
  end
  else
      Result:=-1.0;
  end;
end;

//*************************String type properties*******************************
function PDElementsS(mode:longint; arg:pAnsiChar):pAnsiChar;cdecl;

Var
   ActivePDElement :TPDElement;
   TestString : String;

begin
  Result:=pAnsiChar(AnsiString('0')); // Default return value
  case mode of
  0: begin  // PDElements.Name read
     Result := '';   // return null if not a PD element
      If Assigned(ActiveCircuit[ActiveActor]) Then
        With ActiveCircuit[ActiveActor] Do Begin
            If ActiveCktElement is TPDElement Then Begin
                ActivePDElement := ActiveCktelement as TPDElement;
                With ActivePDElement Do
                     Result := pAnsiChar(AnsiString(Format('%s.%s',[Parentclass.Name, Name])));  // full name
            End;
        End;
  end;
  1: begin  // PDElements.Name write
      If Assigned(ActiveCircuit[ActiveActor]) Then
      With ActiveCircuit[ActiveActor] Do Begin
          TestString := widestring(arg);
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
  end
  else
      Result:=pAnsiChar(AnsiString('Error, parameter not valid'));
  end;
end;

end.
