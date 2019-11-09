unit CAPI_Topology;

{$inline on}

interface

uses
    CAPI_Utils;

function Topology_Get_NumLoops(): Integer; CDECL;
function Topology_Get_ActiveBranch(): Integer; CDECL;
procedure Topology_Get_AllIsolatedBranches(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Topology_Get_AllIsolatedBranches_GR(); CDECL;
procedure Topology_Get_AllLoopedPairs(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Topology_Get_AllLoopedPairs_GR(); CDECL;
function Topology_Get_BackwardBranch(): Integer; CDECL;
function Topology_Get_BranchName(): PAnsiChar; CDECL;
function Topology_Get_First(): Integer; CDECL;
function Topology_Get_ForwardBranch(): Integer; CDECL;
function Topology_Get_LoopedBranch(): Integer; CDECL;
function Topology_Get_Next(): Integer; CDECL;
function Topology_Get_NumIsolatedBranches(): Integer; CDECL;
function Topology_Get_ParallelBranch(): Integer; CDECL;
procedure Topology_Set_BranchName(const Value: PAnsiChar); CDECL;
procedure Topology_Get_AllIsolatedLoads(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure Topology_Get_AllIsolatedLoads_GR(); CDECL;
function Topology_Get_FirstLoad(): Integer; CDECL;
function Topology_Get_NextLoad(): Integer; CDECL;
function Topology_Get_NumIsolatedLoads(): Integer; CDECL;
function Topology_Get_ActiveLevel(): Integer; CDECL;
function Topology_Get_BusName(): PAnsiChar; CDECL;
procedure Topology_Set_BusName(const Value: PAnsiChar); CDECL;

implementation

uses
    CAPI_Constants,
    CktTree,
    DSSGlobals,
    CktElement,
    PDElement,
    PCElement,
    SysUtils,
    DSSClass,
    DSSHelper;

function ActiveTree: TCktTree;
begin
    Result := NIL;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.ActiveCircuit.GetTopology;
end;
//------------------------------------------------------------------------------
function ActiveTreeNode: TCktTreeNode;
var
    topo: TCktTree;
begin
    Result := NIL;
    topo := ActiveTree;
    if assigned(topo) then
        Result := topo.PresentBranch;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumLoops(): Integer; CDECL;
var
    topo: TCktTree;
    pdElem: TPDElement;
begin
    Result := 0;
    topo := ActiveTree;
    if topo <> NIL then
    begin
        Result := 0;
        PDElem := topo.First;
        while Assigned(PDElem) do
        begin
            if topo.PresentBranch.IsLoopedHere then
                Inc(Result);
            PDElem := topo.GoForward;
        end;
    end;
    Result := Result div 2;
end;
//------------------------------------------------------------------------------
function Topology_Get_ActiveBranch(): Integer; CDECL;
var
    topo: TCktTree;
    node: TCktTreeNode;
begin
    Result := 0;
    topo := ActiveTree;
    node := ActiveTreeNode;
    if assigned(node) then
    begin
        Result := topo.Level;
        DSSPrime.ActiveCircuit.ActiveCktElement := node.CktObject;
    end;
end;
//------------------------------------------------------------------------------
procedure Topology_Get_AllIsolatedBranches(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: array of Widestring;
    ActualResult: PPAnsiCharArray;
    elm: TPDElement;
    topo: TCktTree;
    k, i: Integer;
begin
    SetLength(Result, 1);
    Result[0] := 'NONE';
    k := 0;
    topo := ActiveTree;
    if Assigned(topo) then
    begin
        elm := DSSPrime.ActiveCircuit.PDElements.First;
        while assigned(elm) do
        begin
            if elm.IsIsolated then
            begin
                Result[k] := elm.QualifiedName;
                Inc(k);
                if k > 0 then
                    SetLength(Result, (k) + 1);
            end;
            elm := DSSPrime.ActiveCircuit.PDElements.Next;
        end;
    end;

    ActualResult := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
    for i := 0 to Length(Result) - 1 do
    begin
        ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
    end;
    SetLength(Result, 0);
end;

procedure Topology_Get_AllIsolatedBranches_GR(); CDECL;
// Same as Topology_Get_AllIsolatedBranches but uses global result (GR) pointers
begin
    Topology_Get_AllIsolatedBranches(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure Topology_Get_AllLoopedPairs(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: array of Widestring;
    ActualResult: PPAnsiCharArray;
    topo: TCktTree;
    pdElem, pdLoop: TPDElement;
    k, i: Integer;
    found: Boolean;
begin
    SetLength(Result, 1);
    Result[0] := 'NONE';
    k := -1;  // because we always increment by 2!
    topo := ActiveTree;
    if topo <> NIL then
    begin
        PDElem := topo.First;
        while Assigned(PDElem) do
        begin
            if topo.PresentBranch.IsLoopedHere then
            begin
                pdLoop := topo.PresentBranch.LoopLineObj;
        // see if we already found this pair
                found := FALSE;
                i := 1;
                while (i <= k) and (not found) do
                begin
                    if (Result[i - 1] = pdElem.QualifiedName) and (Result[i] = pdLoop.QualifiedName) then
                        found := TRUE;
                    if (Result[i - 1] = pdLoop.QualifiedName) and (Result[i] = pdElem.QualifiedName) then
                        found := TRUE;
                    i := i + 1;
                end;
                if not found then
                begin
                    k := k + 2;
                    SetLength(Result, k + 1);
                    Result[k - 1] := pdElem.QualifiedName;
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

procedure Topology_Get_AllLoopedPairs_GR(); CDECL;
// Same as Topology_Get_AllLoopedPairs but uses global result (GR) pointers
begin
    Topology_Get_AllLoopedPairs(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Topology_Get_BackwardBranch(): Integer; CDECL;
var
    topo: TCktTree;
begin
    Result := 0;
    topo := ActiveTree;
    if assigned(topo) then
    begin
        if assigned(topo.GoBackward) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
            Result := 1;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_BranchName_AnsiString(): Ansistring; inline;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := '';
    node := ActiveTreeNode;
    if assigned(node) then
    begin
        elm := node.CktObject;
        if assigned(elm) then
            Result := elm.QualifiedName;
    end;
end;

function Topology_Get_BranchName(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Topology_Get_BranchName_AnsiString());
end;
//------------------------------------------------------------------------------
function Topology_Get_First(): Integer; CDECL;
var
    topo: TCktTree;
begin
    Result := 0;
    topo := ActiveTree;
    if assigned(topo) then
    begin
        if assigned(topo.First) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
            Result := 1;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ForwardBranch(): Integer; CDECL;
var
    topo: TCktTree;
begin
    Result := 0;
    topo := ActiveTree;
    if assigned(topo) then
    begin
        if assigned(topo.GoForward) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
            Result := 1;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_LoopedBranch(): Integer; CDECL;
var
    node: TCktTreeNode;
begin
    Result := 0;
    node := ActiveTreeNode;
    if assigned(node) then
    begin
        if node.IsLoopedHere then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := node.LoopLineObj;
            Result := 1;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_Next(): Integer; CDECL;
begin
    Result := Topology_Get_ForwardBranch;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumIsolatedBranches(): Integer; CDECL;
var
    elm: TPDElement;
    topo: TCktTree;
begin
    Result := 0;
    topo := ActiveTree;
    if Assigned(topo) then
    begin
        elm := DSSPrime.ActiveCircuit.PDElements.First;
        while assigned(elm) do
        begin
            if elm.IsIsolated then
                Inc(Result);
            elm := DSSPrime.ActiveCircuit.PDElements.Next;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ParallelBranch(): Integer; CDECL;
var
    node: TCktTreeNode;
begin
    Result := 0;
    node := ActiveTreeNode;
    if assigned(node) then
    begin
        if node.IsParallel then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := node.LoopLineObj;
            Result := 1;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Topology_Set_BranchName(const Value: PAnsiChar); CDECL;
var
    topo: TCktTree;
    S: String;
    Found: Boolean;
    elem: TDSSCktElement;
    pdElem: TPDElement;
begin
    Found := FALSE;
    elem := NIL;
    S := Value;  // Convert to Pascal String
    topo := ActiveTree;
    if assigned(topo) then
    begin
        elem := DSSPrime.ActiveCircuit.ActiveCktElement;
        pdElem := topo.First;
        while Assigned(pdElem) do
        begin
            if (CompareText(pdElem.QualifiedName, S) = 0) then
            begin
                DSSPrime.ActiveCircuit.ActiveCktElement := pdElem;
                Found := TRUE;
                Break;
            end;
            pdElem := topo.GoForward;
        end;
    end;
    if not Found then
    begin
        DoSimpleMsg('Branch "' + S + '" Not Found in Active Circuit Topology.', 5003);
        if assigned(elem) then
            DSSPrime.ActiveCircuit.ActiveCktElement := elem;
    end;
end;
//------------------------------------------------------------------------------
procedure Topology_Get_AllIsolatedLoads(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: array of Widestring;
    ActualResult: PPAnsiCharArray;
    elm: TPCElement;
    topo: TCktTree;
    k, i: Integer;
begin
    SetLength(Result, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    k := 0;
    topo := ActiveTree;
    if Assigned(topo) then
    begin
        elm := DSSPrime.ActiveCircuit.PCElements.First;
        while assigned(elm) do
        begin
            if elm.IsIsolated then
            begin
                Result[k] := elm.QualifiedName;
                Inc(k);
                if k > 0 then
                    SetLength(Result, (k) + 1);
            end;
            elm := DSSPrime.ActiveCircuit.PCElements.Next;
        end;
    end;

    ActualResult := DSS_CreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
    for i := 0 to Length(Result) - 1 do
    begin
        ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
    end;
    SetLength(Result, 0);
end;

procedure Topology_Get_AllIsolatedLoads_GR(); CDECL;
// Same as Topology_Get_AllIsolatedLoads but uses global result (GR) pointers
begin
    Topology_Get_AllIsolatedLoads(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Topology_Get_FirstLoad(): Integer; CDECL;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := 0;
    node := ActiveTreeNode;
    if assigned(node) then
    begin
        elm := node.FirstShuntObject;
        if assigned(elm) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := elm;
            Result := 1;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_NextLoad(): Integer; CDECL;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := 0;
    node := ActiveTreeNode;
    if assigned(node) then
    begin
        elm := node.NextShuntObject;
        if assigned(elm) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := elm;
            Result := 1;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumIsolatedLoads(): Integer; CDECL;
var
    elm: TPCElement;
    topo: TCktTree;
begin
    Result := 0;
    topo := ActiveTree;
    if Assigned(topo) then
    begin
        elm := DSSPrime.ActiveCircuit.PCElements.First;
        while assigned(elm) do
        begin
            if elm.IsIsolated then
                Inc(Result);
            elm := DSSPrime.ActiveCircuit.PCElements.Next;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ActiveLevel(): Integer; CDECL;
begin
    Result := Topology_Get_ActiveBranch;
end;
//------------------------------------------------------------------------------
function Topology_Get_BusName_AnsiString(): Ansistring; inline;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := '';
    node := ActiveTreeNode;
    if assigned(node) then
    begin
        elm := node.CktObject;
        if assigned(elm) then
            Result := elm.FirstBus;
    end;
end;

function Topology_Get_BusName(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Topology_Get_BusName_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Topology_Set_BusName(const Value: PAnsiChar); CDECL;
var
    topo: TCktTree;
    S, B: String;
    Found: Boolean;
    elem: TDSSCktElement;
    pdElem: TPDElement;
begin
    Found := FALSE;
    elem := NIL;
    S := Value;  // Convert to Pascal String
    topo := ActiveTree;
    if assigned(topo) then
    begin
        elem := DSSPrime.ActiveCircuit.ActiveCktElement;
        pdElem := topo.First;
        while Assigned(pdElem) and (not found) do
        begin
            B := pdElem.FirstBus;
            while Length(B) > 0 do
            begin
                if (CompareText(B, S) = 0) then
                begin
                    DSSPrime.ActiveCircuit.ActiveCktElement := pdElem;
                    Found := TRUE;
                    Break;
                end;
                B := pdElem.NextBus;
            end;
            pdElem := topo.GoForward;
        end;
    end;
    if not Found then
    begin
        DoSimpleMsg('Bus "' + S + '" Not Found in Active Circuit Topology.', 5003);
        if assigned(elem) then
            DSSPrime.ActiveCircuit.ActiveCktElement := elem;
    end;
end;
//------------------------------------------------------------------------------
end.
