unit CAPI_Topology;

interface

uses
    CAPI_Utils,
    CAPI_Types;

function Topology_Get_NumLoops(): Integer; CDECL;
function Topology_Get_ActiveBranch(): Integer; CDECL;
procedure Topology_Get_AllIsolatedBranches(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Topology_Get_AllIsolatedBranches_GR(); CDECL;
procedure Topology_Get_AllLoopedPairs(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
procedure Topology_Get_AllIsolatedLoads(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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

//------------------------------------------------------------------------------
function ActiveTree(DSS: TDSSContext; out topo: TCktTree): Boolean;
begin
    topo := NIL;
    Result := False;
    if InvalidCircuit(DSS) then
        Exit;
    topo := DSS.ActiveCircuit.GetTopology;
    If (topo = NIL) then
    begin
        if (DSS_CAPI_EXT_ERRORS) then
        begin
            DoSimpleMsg(DSS, _('Topology is not initialized for the active circuit.'), 5097);
        end;
        Exit;
    end;
    Result := True; 
end;
//------------------------------------------------------------------------------
function ActiveTreeNode(DSS: TDSSContext; var node: TCktTreeNode): Boolean;
var
    topo: TCktTree;
begin
    node := NIL;
    Result := False;
    if not ActiveTree(DSS, topo) then
        Exit;
    node := topo.PresentBranch;
    if node = NIL then
    begin
        Exit;
    end;
    Result := True;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumLoops(): Integer; CDECL;
var
    topo: TCktTree;
    pdElem: TPDElement;
begin
    Result := 0;
    if not ActiveTree(DSSPrime, topo) then
        Exit;

    PDElem := topo.First;
    while Assigned(PDElem) do
    begin
        if topo.PresentBranch.IsLoopedHere then
            Inc(Result);
        PDElem := topo.GoForward;
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
    if not ActiveTree(DSSPrime, topo) then
        Exit;
    if not ActiveTreeNode(DSSPrime, node) then
        Exit;
    Result := topo.Level;
    DSSPrime.ActiveCircuit.ActiveCktElement := node.CktObject;
end;
//------------------------------------------------------------------------------
procedure Topology_Get_AllIsolatedBranches(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: array of AnsiString;
    ActualResult: PPAnsiCharArray0;
    elm: TPDElement;
    topo: TCktTree;
    k, i: Integer;
begin
    SetLength(Result, 1);
    k := 0;
    elm := NIL;
    if ActiveTree(DSSPrime, topo) then
        elm := DSSPrime.ActiveCircuit.PDElements.First;

    while assigned(elm) do
    begin
        if Flg.IsIsolated in elm.Flags then
        begin
            Result[k] := elm.FullName;
            Inc(k);
            if k > 0 then
                SetLength(Result, k + 1);
        end;
        elm := DSSPrime.ActiveCircuit.PDElements.Next;
    end;
    if k = 0 then
    begin
        SetLength(Result, 0);
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    ActualResult := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
    for i := 0 to Length(Result) - 1 do
    begin
        ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
    end;
    SetLength(Result, 0);
end;

procedure Topology_Get_AllIsolatedBranches_GR(); CDECL;
// Same as Topology_Get_AllIsolatedBranches but uses global result (GR) pointers
begin
    Topology_Get_AllIsolatedBranches(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure Topology_Get_AllLoopedPairs(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: array of AnsiString;
    ActualResult: PPAnsiCharArray0;
    topo: TCktTree;
    pdElem, pdLoop: TPDElement;
    k, i: Integer;
    found: Boolean;
begin
    SetLength(Result, 1);
    k := -1;  // because we always increment by 2!
    PDElem := NIL;
    if ActiveTree(DSSPrime, topo) then
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
                if (Result[i - 1] = pdElem.FullName) and (Result[i] = pdLoop.FullName) then
                    found := TRUE;
                if (Result[i - 1] = pdLoop.FullName) and (Result[i] = pdElem.FullName) then
                    found := TRUE;
                i := i + 1;
            end;
            if not found then
            begin
                k := k + 2;
                SetLength(Result, k + 1);
                Result[k - 1] := pdElem.FullName;
                Result[k] := pdLoop.FullName;
            end;
        end;
        PDElem := topo.GoForward;
    end;
    
    if k = -1 then
    begin
        SetLength(Result, 0);
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    

    ActualResult := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
    for i := 0 to Length(Result) - 1 do
    begin
        ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
    end;
    SetLength(Result, 0);
end;

procedure Topology_Get_AllLoopedPairs_GR(); CDECL;
// Same as Topology_Get_AllLoopedPairs but uses global result (GR) pointers
begin
    Topology_Get_AllLoopedPairs(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Topology_Get_BackwardBranch(): Integer; CDECL;
var
    topo: TCktTree;
begin
    Result := 0;
    if not ActiveTree(DSSPrime, topo) then
        Exit;

    if assigned(topo.GoBackward) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
        Result := 1;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_BranchName(): PAnsiChar; CDECL;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := NIL;
    if not ActiveTreeNode(DSSPrime, node) then
        Exit;
    elm := node.CktObject;
    if assigned(elm) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elm.FullName);
end;
//------------------------------------------------------------------------------
function Topology_Get_First(): Integer; CDECL;
var
    topo: TCktTree;
begin
    Result := 0;
    if not ActiveTree(DSSPrime, topo) then
        Exit;

    if assigned(topo.First) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
        Result := 1;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ForwardBranch(): Integer; CDECL;
var
    topo: TCktTree;
begin
    Result := 0;
    if not ActiveTree(DSSPrime, topo) then
        Exit;

    if assigned(topo.GoForward) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := topo.PresentBranch.CktObject;
        Result := 1;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_LoopedBranch(): Integer; CDECL;
var
    node: TCktTreeNode;
begin
    Result := 0;
    if not ActiveTreeNode(DSSPrime, node) then
        Exit;
    if node.IsLoopedHere then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := node.LoopLineObj;
        Result := 1;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_Next(): Integer; CDECL;
begin
    Result := Topology_Get_ForwardBranch();
end;
//------------------------------------------------------------------------------
function Topology_Get_NumIsolatedBranches(): Integer; CDECL;
var
    elm: TPDElement;
    topo: TCktTree;
begin
    Result := 0;
    if not ActiveTree(DSSPrime, topo) then
        Exit;

    elm := DSSPrime.ActiveCircuit.PDElements.First;
    while assigned(elm) do
    begin
        if Flg.IsIsolated in elm.Flags then
            Inc(Result);
        elm := DSSPrime.ActiveCircuit.PDElements.Next;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ParallelBranch(): Integer; CDECL;
var
    node: TCktTreeNode;
begin
    Result := 0;
    if not ActiveTreeNode(DSSPrime, node) then
        Exit;
    if node.IsParallel then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := node.LoopLineObj;
        Result := 1;
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
    if InvalidCircuit(DSSPrime) then
        Exit;

    Found := FALSE;
    elem := NIL;
    S := Value;  // Convert to Pascal String
    if ActiveTree(DSSPrime, topo) then
    begin
        elem := DSSPrime.ActiveCircuit.ActiveCktElement;
        pdElem := topo.First;
        while Assigned(pdElem) do
        begin
            if (AnsiCompareText(pdElem.FullName, S) = 0) then
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
        DoSimpleMsg(DSSPrime, 'Branch "%s" not found in Active Circuit Topology.', [S], 5003);
        if assigned(elem) then
            DSSPrime.ActiveCircuit.ActiveCktElement := elem;
    end;
end;
//------------------------------------------------------------------------------
procedure Topology_Get_AllIsolatedLoads(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: array of AnsiString;
    ActualResult: PPAnsiCharArray0;
    elm: TPCElement;
    topo: TCktTree;
    k, i: Integer;
begin
    SetLength(Result, 1);
    k := 0;
    if ActiveTree(DSSPrime, topo) then
    begin
        elm := DSSPrime.ActiveCircuit.PCElements.First;
        while assigned(elm) do
        begin
            if Flg.IsIsolated in elm.Flags then
            begin
                Result[k] := elm.FullName;
                Inc(k);
                if k > 0 then
                    SetLength(Result, (k) + 1);
            end;
            elm := DSSPrime.ActiveCircuit.PCElements.Next;
        end;
    end;
    
    if k = 0 then
    begin
        SetLength(Result, 0);
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    ActualResult := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, Length(Result));
    for i := 0 to Length(Result) - 1 do
    begin
        ActualResult[i] := DSS_CopyStringAsPChar(Result[i]);
    end;
    SetLength(Result, 0);
end;

procedure Topology_Get_AllIsolatedLoads_GR(); CDECL;
// Same as Topology_Get_AllIsolatedLoads but uses global result (GR) pointers
begin
    Topology_Get_AllIsolatedLoads(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Topology_Get_FirstLoad(): Integer; CDECL;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := 0;
    if not ActiveTreeNode(DSSPrime, node) then
        Exit;
    elm := node.FirstShuntObject;
    if assigned(elm) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := elm;
        Result := 1;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_NextLoad(): Integer; CDECL;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := 0;
    if not ActiveTreeNode(DSSPrime, node) then
        Exit;

    elm := node.NextShuntObject;
    if assigned(elm) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := elm;
        Result := 1;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_NumIsolatedLoads(): Integer; CDECL;
var
    elm: TPCElement;
    topo: TCktTree;
begin
    Result := 0;
    if not ActiveTree(DSSPrime, topo) then
        Exit;
        
    elm := DSSPrime.ActiveCircuit.PCElements.First;
    while assigned(elm) do
    begin
        if Flg.IsIsolated in elm.Flags then
            Inc(Result);
        elm := DSSPrime.ActiveCircuit.PCElements.Next;
    end;
end;
//------------------------------------------------------------------------------
function Topology_Get_ActiveLevel(): Integer; CDECL;
begin
    Result := Topology_Get_ActiveBranch();
end;
//------------------------------------------------------------------------------
function Topology_Get_BusName(): PAnsiChar; CDECL;
var
    node: TCktTreeNode;
    elm: TDSSCktElement;
begin
    Result := NIL;
    if not ActiveTreeNode(DSSPrime, node) then
        Exit;
    elm := node.CktObject;
    if assigned(elm) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elm.FirstBus);
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
    if not ActiveTree(DSSPrime, topo) then
        Exit;

    Found := FALSE;
    elem := NIL;
    S := Value;  // Convert to Pascal String
    elem := DSSPrime.ActiveCircuit.ActiveCktElement;
    pdElem := topo.First;
    while Assigned(pdElem) and (not found) do
    begin
        B := pdElem.FirstBus;
        while Length(B) > 0 do
        begin
            if (AnsiCompareText(B, S) = 0) then
            begin
                DSSPrime.ActiveCircuit.ActiveCktElement := pdElem;
                Found := TRUE;
                Break;
            end;
            B := pdElem.NextBus;
        end;
        pdElem := topo.GoForward;
    end;
    if not Found then
    begin
        DoSimpleMsg(DSSPrime, 'Bus "%s" not found in Active Circuit Topology.', [S], 5003);
        if assigned(elem) then
            DSSPrime.ActiveCircuit.ActiveCktElement := elem;
    end;
end;
//------------------------------------------------------------------------------
end.
