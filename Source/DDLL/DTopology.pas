unit DTopology;

interface

function TopologyI(mode: Longint; arg: Longint): Longint; CDECL;
function TopologyS(mode: Longint; arg: Pansichar): Pansichar; CDECL;
procedure TopologyV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

implementation

uses
    CktTree,
    DSSGlobals,
    CktElement,
    PDElement,
    PCElement,
    Variants,
    SysUtils
{$IFNDEF FPC_DLL}
    ,
    Dialogs
{$ENDIF}
    ;

function ActiveTree: TCktTree;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].GetTopology;
end;

function ActiveTreeNode: TCktTreeNode;
var
    topo: TCktTree;
begin
    Result := NIL;
    topo := ActiveTree;
    if assigned(topo) then
        Result := topo.PresentBranch;
end;

function ForwardBranch: Integer;
var
    topo: TCktTree;
begin
    Result := 0;
    topo := ActiveTree;
    if assigned(topo) then
    begin
        if assigned(topo.GoForward) then
        begin
            ActiveCircuit[ActiveActor].ActiveCktElement := topo.PresentBranch.CktObject;
            Result := 1;
        end;
    end;
end;

function ActiveBranch: Integer;
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
        ActiveCircuit[ActiveActor].ActiveCktElement := node.CktObject;
    end;
end;

function TopologyI(mode: Longint; arg: Longint): Longint; CDECL;

var
    topo: TCktTree;
    pdElem: TPDElement;
    elm: TPDElement;
    node: TCktTreeNode;

begin
    Result := 0;         // Default return value
    case mode of
        0:
        begin  // Topology.NumLoops
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
        1:
        begin  // Topology.NumIsolatedBranches
            topo := ActiveTree;
            if Assigned(topo) then
            begin
                elm := ActiveCircuit[ActiveActor].PDElements.First;
                while assigned(elm) do
                begin
                    if elm.IsIsolated then
                        Inc(Result);
                    elm := ActiveCircuit[ActiveActor].PDElements.Next;
                end;
            end;
        end;
        2:
        begin  // Topology.NumIsolatedLoads
            topo := ActiveTree;
            if Assigned(topo) then
            begin
                elm := ActiveCircuit[ActiveActor].PCElements.First;
                while assigned(elm) do
                begin
                    if elm.IsIsolated then
                        Inc(Result);
                    elm := ActiveCircuit[ActiveActor].PCElements.Next;
                end;
            end;
        end;
        3:
        begin  // Topology.First
            topo := ActiveTree;
            if assigned(topo) then
            begin
                if assigned(topo.First) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := topo.PresentBranch.CktObject;
                    Result := 1;
                end;
            end;
        end;
        4:
        begin  // Topology.Next
            Result := ForwardBranch;
        end;
        5:
        begin  // Topology.ActiveBranch
            Result := ActiveBranch;
        end;
        6:
        begin  // Topology.ForwardBranch
            Result := ForWardBranch;
        end;
        7:
        begin  // Topology.BackwardBranch
            topo := ActiveTree;
            if assigned(topo) then
            begin
                if assigned(topo.GoBackward) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := topo.PresentBranch.CktObject;
                    Result := 1;
                end;
            end;
        end;
        8:
        begin  // Topology.LoopedBranch
            node := ActiveTreeNode;
            if assigned(node) then
            begin
                if node.IsLoopedHere then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := node.LoopLineObj;
                    Result := 1;
                end;
            end;
        end;
        9:
        begin  // Topology.ParallelBranch
            node := ActiveTreeNode;
            if assigned(node) then
            begin
                if node.IsParallel then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := node.LoopLineObj;
                    Result := 1;
                end;
            end;
        end;
        10:
        begin  // Topology.FirstLoad
            node := ActiveTreeNode;
            if assigned(node) then
            begin
                elm := node.FirstShuntObject;
                if assigned(elm) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elm;
                    Result := 1;
                end;
            end;
        end;
        11:
        begin  // Topology.NextLoad
            node := ActiveTreeNode;
            if assigned(node) then
            begin
                elm := node.NextShuntObject;
                if assigned(elm) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := elm;
                    Result := 1;
                end;
            end;
        end;
        12:
        begin  // Topology.ActiveLevel
            Result := ActiveBranch;
        end
    else
        Result := -1;
    end;
end;

//****************************String type properties*****************************
function TopologyS(mode: Longint; arg: Pansichar): Pansichar; CDECL;

var
    node: TCktTreeNode;
    elm: TDSSCktElement;
    topo: TCktTree;
    S, B: String;
    Found: Boolean;
    elem: TDSSCktElement;
    pdElem: TPDElement;

begin
    Result := Pansichar(Ansistring(''));  // Default return value
    case mode of
        0:
        begin  // Topology.BranchName read
            Result := Pansichar(Ansistring(''));
            node := ActiveTreeNode;
            if assigned(node) then
            begin
                elm := node.CktObject;
                if assigned(elm) then
                    Result := Pansichar(Ansistring(elm.QualifiedName));
            end;
        end;
        1:
        begin  // Topology.BranchName write
            Found := FALSE;
            elem := NIL;
            S := String(arg);  // Convert to Pascal String
            topo := ActiveTree;
            if assigned(topo) then
            begin
                elem := ActiveCircuit[ActiveActor].ActiveCktElement;
                pdElem := topo.First;
                while Assigned(pdElem) do
                begin
                    if (CompareText(pdElem.QualifiedName, S) = 0) then
                    begin
                        ActiveCircuit[ActiveActor].ActiveCktElement := pdElem;
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
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
            end;
        end;
        2:
        begin  // Topology.BusName read
            Result := Pansichar(Ansistring(''));
            node := ActiveTreeNode;
            if assigned(node) then
            begin
                elm := node.CktObject;
                if assigned(elm) then
                    Result := Pansichar(Ansistring(elm.FirstBus));
            end;
        end;
        3:
        begin  // Topology.BusName write
            Found := FALSE;
            elem := NIL;
            S := String(arg);  // Convert to Pascal String
            topo := ActiveTree;
            if assigned(topo) then
            begin
                elem := ActiveCircuit[ActiveActor].ActiveCktElement;
                pdElem := topo.First;
                while Assigned(pdElem) and (not found) do
                begin
                    B := pdElem.FirstBus;
                    while Length(B) > 0 do
                    begin
                        if (CompareText(B, S) = 0) then
                        begin
                            ActiveCircuit[ActiveActor].ActiveCktElement := pdElem;
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
                    ActiveCircuit[ActiveActor].ActiveCktElement := elem;
            end;
        end
    else
        Result := Pansichar(Ansistring('Error, parameter not valid'));
    end;
end;

//****************************Variant type properties*****************************
procedure TopologyV(mode: Longint; var myPointer: Pointer; var myType, mySize: Longint); CDECL;

var
    topo: TCktTree;
    pdElem,
    pdLoop: TPDElement;
    k,
    i: Integer;
    found: Boolean;
    elm: TPDElement;
    TStr: array of String;

begin
    case mode of
        0:
        begin  // Topology.AllLoopedPairs
            myType := 4;        // String
            setlength(myStrArray, 0);
            setlength(TStr, 1);
            TStr[0] := 'NONE';
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
                            if (TStr[i - 1] = pdElem.QualifiedName) and (TStr[i] = pdLoop.QualifiedName) then
                                found := TRUE;
                            if (TStr[i - 1] = pdLoop.QualifiedName) and (TStr[i] = pdElem.QualifiedName) then
                                found := TRUE;
                            i := i + 1;
                        end;
                        if not found then
                        begin
                            k := k + 2;
                            setlength(TStr, k + 1);
                            TStr[k - 1] := pdElem.QualifiedName;
                            TStr[k] := pdLoop.QualifiedName;
                        end;
                    end;
                    PDElem := topo.GoForward;
                end;
            end;
            if (length(TStr) > 0) then
            begin
                for i := 0 to High(TStr) do
                begin
                    if TStr[i] <> '' then
                    begin
                        WriteStr2Array(TStr[i]);
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        1:
        begin  // Topology.AllIsolatedBranches
            myType := 4;        // String
            setlength(myStrArray, 0);
            setlength(TStr, 1);
            TStr[0] := 'NONE';
            k := 0;
            topo := ActiveTree;
            if Assigned(topo) then
            begin
                elm := ActiveCircuit[ActiveActor].PDElements.First;
                while assigned(elm) do
                begin
                    if elm.IsIsolated then
                    begin
                        TStr[k] := elm.QualifiedName;
                        Inc(k);
                        if k > 0 then
                            setlength(TStr, k + 1);
                    end;
                    elm := ActiveCircuit[ActiveActor].PDElements.Next;
                end;
            end;
            if (length(TStr) > 0) then
            begin
                for i := 0 to High(TStr) do
                begin
                    if TStr[i] <> '' then
                    begin
                        WriteStr2Array(TStr[i]);
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end;
        2:
        begin  // Topology.AllIsolatedLoads
            myType := 4;        // String
            setlength(myStrArray, 0);
            setlength(TStr, 1);
            TStr[0] := 'NONE';
            k := 0;
            topo := ActiveTree;
            if Assigned(topo) then
            begin
                elm := ActiveCircuit[ActiveActor].PCElements.First;
                while assigned(elm) do
                begin
                    if elm.IsIsolated then
                    begin
                        TStr[k] := elm.QualifiedName;
                        Inc(k);
                        if k > 0 then
                            setlength(TStr, k + 1);
                    end;
                    elm := ActiveCircuit[ActiveActor].PCElements.Next;
                end;
            end;
            if (length(TStr) > 0) then
            begin
                for i := 0 to High(TStr) do
                begin
                    if TStr[i] <> '' then
                    begin
                        WriteStr2Array(TStr[i]);
                        WriteStr2Array(Char(0));
                    end;
                end;
            end;
            if (length(myStrArray) = 0) then
                WriteStr2Array('None');
            myPointer := @(myStrArray[0]);
            mySize := Length(myStrArray);
        end
    else
    begin
        myType := 4;        // String
        setlength(myStrArray, 0);
        WriteStr2Array('Error, parameter not recognized');
        myPointer := @(myStrArray[0]);
        mySize := Length(myStrArray);
    end;
    end;
end;

end.
