UNIT CAPI_Topology;
{$inline on}

INTERFACE

USES CAPI_Utils;

function Topology_Get_NumLoops():Integer;cdecl;
function Topology_Get_ActiveBranch():Integer;cdecl;
PROCEDURE Topology_Get_AllIsolatedBranches(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Topology_Get_AllIsolatedBranches_GR();cdecl;
PROCEDURE Topology_Get_AllLoopedPairs(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Topology_Get_AllLoopedPairs_GR();cdecl;
function Topology_Get_BackwardBranch():Integer;cdecl;
function Topology_Get_BranchName():PAnsiChar;cdecl;
function Topology_Get_First():Integer;cdecl;
function Topology_Get_ForwardBranch():Integer;cdecl;
function Topology_Get_LoopedBranch():Integer;cdecl;
function Topology_Get_Next():Integer;cdecl;
function Topology_Get_NumIsolatedBranches():Integer;cdecl;
function Topology_Get_ParallelBranch():Integer;cdecl;
procedure Topology_Set_BranchName(const Value: PAnsiChar);cdecl;
PROCEDURE Topology_Get_AllIsolatedLoads(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Topology_Get_AllIsolatedLoads_GR();cdecl;
function Topology_Get_FirstLoad():Integer;cdecl;
function Topology_Get_NextLoad():Integer;cdecl;
function Topology_Get_NumIsolatedLoads():Integer;cdecl;
function Topology_Get_ActiveLevel():Integer;cdecl;
function Topology_Get_BusName():PAnsiChar;cdecl;
procedure Topology_Set_BusName(const Value: PAnsiChar);cdecl;

IMPLEMENTATION

USES CAPI_Constants, CktTree, DSSGlobals, CktElement, PDElement, PCElement, SysUtils;

function ActiveTree: TCktTree;
begin
  Result := nil;
  if ActiveCircuit[ActiveActor] <> Nil then Result := ActiveCircuit[ActiveActor].GetTopology;
end;
//------------------------------------------------------------------------------
function ActiveTreeNode: TCktTreeNode;
var
  topo: TCktTree;
begin
  Result := nil;
  topo := ActiveTree;
  if assigned(topo) then Result := topo.PresentBranch;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumLoops():Integer;cdecl;
var
  topo: TCktTree;
  pdElem: TPDElement;
begin
  Result := 0;
  topo := ActiveTree;
  if topo <> nil then begin
    Result := 0;
    PDElem := topo.First;
    While Assigned (PDElem) do begin
      if topo.PresentBranch.IsLoopedHere then Inc(Result);
      PDElem := topo.GoForward;
    end;
  end;
  Result := Result div 2;
end;
//------------------------------------------------------------------------------
function Topology_Get_ActiveBranch():Integer;cdecl;
var
  topo: TCktTree;
  node: TCktTreeNode;
begin
  Result := 0;
  topo := ActiveTree;
  node := ActiveTreeNode;
  if assigned(node) then begin
    Result := topo.Level;
    ActiveCircuit[ActiveActor].ActiveCktElement := node.CktObject;
  end;
end;
//------------------------------------------------------------------------------
PROCEDURE Topology_Get_AllIsolatedBranches(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: Array of WideString;
  ActualResult: PPAnsiCharArray;
  elm: TPDElement;
  topo: TCktTree;
  k, i: integer;
begin
  SetLength(Result, 1);
  Result[0] := 'NONE';
  k := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit[ActiveActor].PDElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then begin
        Result[k] := elm.QualifiedName;
        Inc(k);
        if k > 0 then SetLength(Result, (k) + 1);
      end;
      elm := ActiveCircuit[ActiveActor].PDElements.Next;
    end;
  end;
  
  ActualResult := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
  for i := 0 to Length(Result) - 1 do
  begin
    ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
  end;
  SetLength(Result, 0);
end;
PROCEDURE Topology_Get_AllIsolatedBranches_GR();cdecl;
// Same as Topology_Get_AllIsolatedBranches but uses global result (GR) pointers
begin
   Topology_Get_AllIsolatedBranches(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
PROCEDURE Topology_Get_AllLoopedPairs(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: Array of WideString;
  ActualResult: PPAnsiCharArray;
  topo: TCktTree;
  pdElem, pdLoop: TPDElement;
  k, i: integer;
  found: boolean;
begin
  SetLength(Result, 1);
  Result[0] := 'NONE';
  k := -1;  // because we always increment by 2!
  topo := ActiveTree;
  if topo <> nil then begin
    PDElem := topo.First;
    While Assigned (PDElem) do begin
      if topo.PresentBranch.IsLoopedHere then begin
        pdLoop := topo.PresentBranch.LoopLineObj;
        // see if we already found this pair
        found := False;
        i := 1;
        while (i <= k) and (not found) do begin
          if (Result[i-1] = pdElem.QualifiedName) and (Result[i] = pdLoop.QualifiedName) then found := True;
          if (Result[i-1] = pdLoop.QualifiedName) and (Result[i] = pdElem.QualifiedName) then found := True;
          i := i + 1;
        end;
        if not found then begin
          k := k + 2;
          SetLength(Result, k + 1);
          Result[k-1] := pdElem.QualifiedName;
          Result[k] := pdLoop.QualifiedName;
        end;
      end;
      PDElem := topo.GoForward;
    end;
  end;

  ActualResult := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
  for i := 0 to Length(Result) - 1 do
  begin
    ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
  end;
  SetLength(Result, 0);
end;
PROCEDURE Topology_Get_AllLoopedPairs_GR();cdecl;
// Same as Topology_Get_AllLoopedPairs but uses global result (GR) pointers
begin
   Topology_Get_AllLoopedPairs(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Topology_Get_BackwardBranch():Integer;cdecl;
var
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if assigned (topo) then begin
    if assigned(topo.GoBackward) then begin
      ActiveCircuit[ActiveActor].ActiveCktElement := topo.PresentBranch.CktObject;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_BranchName_AnsiString():AnsiString;inline;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := '';
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.CktObject;
    if assigned(elm) then Result := elm.QualifiedName;
  end;
end;

function Topology_Get_BranchName():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Topology_Get_BranchName_AnsiString());
end;
//------------------------------------------------------------------------------
function Topology_Get_First():Integer;cdecl;
var
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if assigned (topo) then begin
    if assigned(topo.First) then begin
      ActiveCircuit[ActiveActor].ActiveCktElement := topo.PresentBranch.CktObject;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ForwardBranch():Integer;cdecl;
var
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if assigned (topo) then begin
    if assigned(topo.GoForward) then begin
      ActiveCircuit[ActiveActor].ActiveCktElement := topo.PresentBranch.CktObject;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_LoopedBranch():Integer;cdecl;
var
  node: TCktTreeNode;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    if node.IsLoopedHere then begin
      ActiveCircuit[ActiveActor].ActiveCktElement := node.LoopLineObj;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_Next():Integer;cdecl;
begin
  Result := Topology_Get_ForwardBranch;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumIsolatedBranches():Integer;cdecl;
var
  elm: TPDElement;
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit[ActiveActor].PDElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then Inc (Result);
      elm := ActiveCircuit[ActiveActor].PDElements.Next;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ParallelBranch():Integer;cdecl;
var
  node: TCktTreeNode;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    if node.IsParallel then begin
      ActiveCircuit[ActiveActor].ActiveCktElement := node.LoopLineObj;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------
procedure Topology_Set_BranchName(const Value: PAnsiChar);cdecl;
var
  topo: TCktTree;
  S: String;
  Found :Boolean;
  elem: TDSSCktElement;
  pdElem: TPDElement;
begin
  Found := FALSE;
  elem := nil;
  S := Value;  // Convert to Pascal String
  topo := ActiveTree;
  if assigned(topo) then begin
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    pdElem := topo.First;
    while Assigned (pdElem) do begin
      if (CompareText(pdElem.QualifiedName, S) = 0) then begin
        ActiveCircuit[ActiveActor].ActiveCktElement := pdElem;
        Found := TRUE;
        Break;
      End;
      pdElem := topo.GoForward;
    end;
  end;
  if not Found then Begin
    DoSimpleMsg('Branch "'+S+'" Not Found in Active Circuit Topology.', 5003);
    if assigned(elem) then ActiveCircuit[ActiveActor].ActiveCktElement := elem;
  end;
end;
//------------------------------------------------------------------------------
PROCEDURE Topology_Get_AllIsolatedLoads(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: Array of WideString;
  ActualResult: PPAnsiCharArray;
  elm: TPCElement;
  topo: TCktTree;
  k, i: integer;
begin
  SetLength(Result, 1);
  Result[0] := DSS_CopyStringAsPChar('NONE');
  k := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit[ActiveActor].PCElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then begin
        Result[k] := elm.QualifiedName;
        Inc(k);
        if k > 0 then SetLength(Result, (k) + 1);
      end;
      elm := ActiveCircuit[ActiveActor].PCElements.Next;
    end;
  end;
  
  ActualResult := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
  for i := 0 to Length(Result) - 1 do
  begin
    ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
  end;
  SetLength(Result, 0);
end;
PROCEDURE Topology_Get_AllIsolatedLoads_GR();cdecl;
// Same as Topology_Get_AllIsolatedLoads but uses global result (GR) pointers
begin
   Topology_Get_AllIsolatedLoads(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Topology_Get_FirstLoad():Integer;cdecl;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.FirstShuntObject;
    if assigned(elm) then begin
      ActiveCircuit[ActiveActor].ActiveCktElement := elm;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_NextLoad():Integer;cdecl;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := 0;
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.NextShuntObject;
    if assigned(elm) then begin
      ActiveCircuit[ActiveActor].ActiveCktElement := elm;
      Result := 1;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumIsolatedLoads():Integer;cdecl;
var
  elm: TPCElement;
  topo: TCktTree;
begin
  Result := 0;
  topo := ActiveTree;
  if Assigned(topo) then begin
    elm := ActiveCircuit[ActiveActor].PCElements.First;
    while assigned (elm) do begin
      if elm.IsIsolated then Inc (Result);
      elm := ActiveCircuit[ActiveActor].PCElements.Next;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ActiveLevel():Integer;cdecl;
begin
  Result := Topology_Get_ActiveBranch;
end;
//------------------------------------------------------------------------------
function Topology_Get_BusName_AnsiString():AnsiString;inline;
var
  node: TCktTreeNode;
  elm: TDSSCktElement;
begin
  Result := '';
  node := ActiveTreeNode;
  if assigned(node) then begin
    elm := node.CktObject;
    if assigned(elm) then Result := elm.FirstBus;
  end;
end;

function Topology_Get_BusName():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Topology_Get_BusName_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Topology_Set_BusName(const Value: PAnsiChar);cdecl;
var
  topo: TCktTree;
  S, B: String;
  Found :Boolean;
  elem: TDSSCktElement;
  pdElem: TPDElement;
begin
  Found := FALSE;
  elem := nil;
  S := Value;  // Convert to Pascal String
  topo := ActiveTree;
  if assigned(topo) then begin
    elem := ActiveCircuit[ActiveActor].ActiveCktElement;
    pdElem := topo.First;
    while Assigned (pdElem) and (not found) do begin
      B := pdElem.FirstBus;
      while Length(B) > 0 do begin
        if (CompareText(B, S) = 0) then begin
          ActiveCircuit[ActiveActor].ActiveCktElement := pdElem;
          Found := TRUE;
          Break;
        end;
        B := pdElem.NextBus;
      end;
      pdElem := topo.GoForward;
    end;
  end;
  if not Found then Begin
    DoSimpleMsg('Bus "'+S+'" Not Found in Active Circuit Topology.', 5003);
    if assigned(elem) then ActiveCircuit[ActiveActor].ActiveCktElement := elem;
  end;
end;
//------------------------------------------------------------------------------
END.
