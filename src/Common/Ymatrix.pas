unit Ymatrix;

// ----------------------------------------------------------
// Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------


interface

uses
    UComplex, DSSUcomplex,
    ucMatrix,
    SysUtils,
    Circuit,
    DSSClass;


// Options for building Y matrix
const
    SERIESONLY = 1;
    WHOLEMATRIX = 2;

type
    EEsolv32Problem = class(Exception);


procedure BuildYMatrix(DSS: TDSSContext; BuildOption: Integer; AllocateVI: Boolean);
procedure ResetSparseMatrix(var hY: NativeUint; size: Integer);
procedure InitializeNodeVbase(ckt: TDSSCircuit);
function CheckYMatrixforZeroes(ckt: TDSSCircuit): String;

implementation

uses
    DSSGlobals,
    CktElement,
    Utilities,
    KLUSolve,
    Solution,
    DSSClassDefs,
    GUtil,
    GSet,
    DSSHelper,
    Bus;


type 
    TCoordLess = TLess<QWord>;
    TCoordSet = TSet<QWord, TCoordLess>;
    TNodeLess = TLess<Integer>;
    TNodeSet = TSet<Integer, TNodeLess>;

procedure ReCalcAllYPrims(Ckt: TDSSCircuit);

var
    pElem: TDSSCktElement;

begin
    if Ckt.LogEvents then
        Ckt.DSS.LogThisEvent(_('Recalc All Yprims'));
    for pElem in Ckt.CktElements do
    begin
        pElem.CalcYPrim();
    end;
end;

procedure ReCalcInvalidYPrims(Ckt: TDSSCircuit);
// Recalc YPrims only for those circuit elements that have had changes since last solution
var
    pElem: TDSSCktElement;
begin
    if Ckt.LogEvents then
        Ckt.DSS.LogThisEvent(_('Recalc Invalid Yprims'));

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
    for pElem in Ckt.IncrCktElements do
    begin
        if pElem.YprimInvalid then
        begin
            pElem.CalcYPrim();
        end;
    end;
{$ENDIF}
    for pElem in Ckt.CktElements do
    begin
        if pElem.YprimInvalid then // or ((DSSObjType and CLASSMASK) = LOAD_ELEMENT)
        begin
            pElem.CalcYPrim();
        end;
    end;
end;


procedure ResetSparseMatrix(var hY: NativeUint; size: Integer);
begin
    if hY <> 0 then
    begin
        if DeleteSparseSet(hY) < 1 then // Get rid of existing one before making a new one
            raise EEsolv32Problem.Create(_('Error Deleting System Y Matrix in ResetSparseMatrix. Problem with Sparse matrix solver.'));

        hY := 0;
    end;

     // Make a new sparse set
    hY := NewSparseSet(Size);
    if hY < 1 then
    begin
        raise EEsolv32Problem.Create(_('Error Creating System Y Matrix. Problem WITH Sparse matrix solver.'));
    end;
end;


procedure InitializeNodeVbase(ckt: TDSSCircuit);
var
    i: Integer;
begin
    if ckt.Solution.NodeVbase = NIL then
    begin
        DoSimpleMsg(ckt.DSS, _('General error: internal NodeVbase is NIL. Please check your input data and retry.'), 11002);
        ckt.DSS.SolutionAbort := True;
        Exit;
    end;

    for i := 1 to ckt.NumNodes do
        ckt.Solution.NodeVbase[i] := ckt.Buses[ckt.MapNodeToBus[i].BusRef].kVBase * 1000.0;
    ckt.Solution.VoltageBaseChanged := FALSE;
end;

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
function UpdateYMatrix(Ckt: TDSSCircuit; BuildOption: Integer; AllocateVI: Boolean): Boolean;
var
    IncrYprim: TCMatrix;
    pElem: TDSSCktElement;
    changedElements: TCoordSet; // elements from the matrix that have been changed
    changedNodes: TNodeSet; // nodes which have affected elements
    coordIt: TCoordSet.TIterator;
    
    i, j, inode, jnode: Integer;
    abortIncremental: Boolean;
    val: Complex;
begin
    changedElements := TCoordSet.Create;
    changedNodes := TNodeSet.Create;
    Result := False;
    IncrYprim := NIL;
    abortIncremental := False;

    // Incremental Y update, only valid for BuildOption = WHOLEMATRIX.
    for pElem in Ckt.IncrCktElements do
    begin
        if (pElem.Enabled and (pElem.Yprim = NIL)) then
        begin
            abortIncremental := True;
            break;
        end;

        if (not pElem.Enabled) or (pElem.Yprim = NIL) then
            continue;

        if IncrYprim <> NIL then
        begin
            IncrYprim.Free;
            IncrYprim := NIL;
        end;
        
        IncrYprim := TCmatrix.CreateMatrix(pElem.Yprim.order);
        IncrYprim.CopyFrom(pElem.Yprim);
        IncrYprim.Negate;
        
        pElem.CalcYPrim();
        
        if (pElem.Yprim = NIL) or (IncrYprim.order <> pElem.Yprim.order) then
        begin
            abortIncremental := True;
            break;
        end;
        
        IncrYprim.AddFrom(pElem.YPrim);
        for i := 1 to pElem.Yprim.order do
        begin
            inode := pElem.NodeRef[i];
            if inode = 0 then continue;
            for j := 1 to pElem.Yprim.order do
            begin
                jnode := pElem.NodeRef[j];
                if jnode = 0 then continue;
                
                val := IncrYprim[i, j];
                if (val.re <> 0) or (val.im <> 0) then
                begin
                    changedNodes.Insert(inode);
                    changedNodes.Insert(jnode);
                    // Encode the coordinates as a 64-bit integer
                    changedElements.Insert((QWord(inode) shl 32) or QWord(jnode));
                end;
            end;
        end;
    end;

    if IncrYprim <> NIL then
    begin
        IncrYprim.Free;
        IncrYprim := NIL;
    end;

    if not abortIncremental then
    begin
        coordIt := changedElements.Min;
        if coordIt <> nil then
        repeat
            // Zeroise only the exact elements affected to make it faster
            if ZeroiseMatrixElement(Ckt.Solution.hYsystem, (coordIt.Data shr 32), coordIt.Data and $FFFFFFFF) = 0 then
            begin
                // If the element doesn't exist in the current compressed matrix, abort!
                abortIncremental := True;
                break;
            end;
        until not coordIt.Next();
    end;

    for pElem in Ckt.CktElements do
    begin
        if abortIncremental then break;

        if (not pElem.Enabled) or (pElem.Yprim = NIL) then
            continue;

        for i := 1 to pElem.Yprim.order do
        begin
            inode := pElem.NodeRef[i];
            if inode = 0 then continue;
            if changedNodes.Find(inode) = NIL then
                // nothing changed for node "inode", we can skip it completely
                continue;

            for j := 1 to pElem.Yprim.order do
            begin
                jnode := pElem.NodeRef[j];
                if jnode = 0 then continue;

                if (changedElements.Find((QWord(inode) shl 32) or (QWord(jnode))) = nil) then
                    continue;

                val := pElem.Yprim[i, j];
                if (val.re = 0) and (val.im = 0) then continue;

                if IncrementMatrixElement(Ckt.Solution.hYsystem, inode, jnode, val.re, val.im) = 0 then
                begin
                    abortIncremental := True;
                    break;
                end;
            end;

            if abortIncremental then break;
        end;

        if abortIncremental then break;
    end;

    if abortIncremental then
    begin
        Result := False;

        // Retry with the full matrix
        Ckt.Solution.SystemYChanged := True;
        BuildYMatrix(Ckt.DSS, BuildOption, AllocateVI);
        Ckt.IncrCktElements.Clear;
    end
    else
    begin
        Ckt.IncrCktElements.Clear;
        Result := True;
    end;

    changedElements.Free;
    changedNodes.Free;
end;
{$ENDIF} //DSS_CAPI_INCREMENTAL_Y

procedure BuildYMatrix(DSS: TDSSContext; BuildOption: Integer; AllocateVI: Boolean);

// Builds designated Y matrix for system and allocates solution arrays

var
    YMatrixsize: Integer;
    CmatArray: pComplexArray;
    pElem: TDSSCktElement;
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
    Incremental: Boolean;
{$ENDIF}

begin
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
    Incremental := False;
{$ENDIF}

    CmatArray := NIL;
    with DSS.ActiveCircuit, Solution do
    begin
        if PreserveNodeVoltages then
            UpdateVBus(); // Update voltage values stored with Bus object

     // the following re counts the number of buses and resets meter zones and feeders
     // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
        if (BusNameRedefined) then
            ReprocessBusDefs;      // This changes the node references into the system Y matrix!!

        YMatrixSize := NumNodes;

        case BuildOption of
            WHOLEMATRIX:
            begin
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
                Incremental := (Solution.SolverOptions <> ord(TSolverOptions.ReuseNothing)) and 
                    (not SystemYChanged) and 
                    (IncrCktElements.Count <> 0) and 
                    (not AllocateVI) and 
                    (not FrequencyChanged);

                if not Incremental then
                begin
                    if IncrCktElements.Count <> 0 then
                        SystemYChanged := True;
{$ENDIF}
                    ResetSparseMatrix(hYsystem, YMatrixSize);
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
                    KLUSolve.SetOptions(hYsystem, SolverOptions);
                end;
{$ENDIF}
                hY := hYsystem;
            end;
            SERIESONLY:
            begin
                ResetSparseMatrix(hYseries, YMatrixSize);
{$IFDEF DSS_CAPI_INCREMENTAL_Y}                
                KLUSolve.SetOptions(hYsystem, SolverOptions);
{$ENDIF}
                hY := hYSeries;
            end;
        end;

     // tune up the Yprims if necessary
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        if not Incremental then 
{$ENDIF}
        begin
            if (FrequencyChanged) then
                ReCalcAllYPrims(DSS.ActiveCircuit)
            else
                ReCalcInvalidYPrims(DSS.ActiveCircuit);
        end;
        
        if DSS.SolutionAbort then
        begin
            DoSimpleMsg(DSS, _('Y matrix build aborted due to error in primitive Y calculations.'), 11001);
            Exit;  // Some problem occured building Yprims
        end;

        FrequencyChanged := FALSE;

        if LogEvents then
            case BuildOption of
                WHOLEMATRIX:
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
                    if Incremental then
                        DSS.LogThisEvent(_('Building Whole Y Matrix -- using incremental method'))
                    else
{$ENDIF}
                        DSS.LogThisEvent(_('Building Whole Y Matrix'));

                SERIESONLY:
                    DSS.LogThisEvent(_('Building Series Y Matrix'));
            end;
          // Add in Yprims for all devices
          
        // Full method, handles all elements
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        if not Incremental then
        begin
{$ENDIF}
            // Full method, handles all elements
            for pElem in CktElements do
            begin
                if not pElem.Enabled then
                    continue;

                // Add stuff only if enabled
                case BuildOption of
                    WHOLEMATRIX:
                        CmatArray := pElem.GetYPrimValues(ALL_YPRIM);
                    SERIESONLY:
                        CmatArray := pElem.GetYPrimValues(SERIES)
                end;
                // new function adding primitive Y matrix to KLU system Y matrix
                if CMatArray <> NIL then
                    if AddPrimitiveMatrix(hY, pElem.Yorder, PLongWord(@pElem.NodeRef[1]), @CMatArray[1]) < 1 then
                        raise EEsolv32Problem.Create(_('Node index out of range adding to System Y Matrix'))
            end;
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        end // if not Incremental
        else
        begin // if Incremental 
            if not UpdateYMatrix(DSS.ActiveCircuit, BuildOption, AllocateVI) then
                Exit;
        end;
{$ENDIF}

     // Allocate voltage and current vectors if requested
        if AllocateVI then
        begin
            if LogEvents then
                DSS.LogThisEvent(_('Reallocating Solution Arrays'));
            ReAllocMem(NodeV, SizeOf(NodeV[1]) * (NumNodes + 1)); // Allocate System Voltage array - allow for zero element
            NodeV[0] := 0;
            ReAllocMem(Currents, SizeOf(Complex) * (NumNodes + 1)); // Allocate System current array
            ReAllocMem(AuxCurrents, SizeOf(Complex) * (NumNodes + 1)); // Allocate System current array
            if (VMagSaved <> NIL) then
                ReallocMem(VMagSaved, 0);
            if (ErrorSaved <> NIL) then
                ReallocMem(ErrorSaved, 0);
            if (NodeVBase <> NIL) then
                ReallocMem(NodeVBase, 0);
            VMagSaved := AllocMem(Sizeof(Double) * NumNodes);  // zero fill
            ErrorSaved := AllocMem(Sizeof(Double) * NumNodes);  // zero fill
            NodeVBase := AllocMem(Sizeof(Double) * NumNodes);  // zero fill
            InitializeNodeVbase(DSS.ActiveCircuit);
{$IFDEF DSS_CAPI_ADIAKOPTICS}
            // A-Diakoptics vectors memory allocation
            ReAllocMem(Node_dV, SizeOf(Node_dV[1]) * (NumNodes + 1)); // Allocate the partial solution voltage
            ReAllocMem(Ic_Local, SizeOf(Ic_Local[1]) * (NumNodes + 1)); // Allocate the Complementary currents
{$ENDIF}
        end;

        case BuildOption of
            WHOLEMATRIX:
            begin
                SeriesYInvalid := TRUE;  // Indicate that the Series matrix may not match
                SystemYChanged := FALSE;
            end;
            SERIESONLY:
                SeriesYInvalid := FALSE;  // SystemYChange unchanged
        end;

        // Deleted RCD only done now on mode change
        // SolutionInitialized := False;  //Require initialization of voltages if Y changed
        if PreserveNodeVoltages then
            RestoreNodeVfromVbus;

    end;
end;

// leave the call to GetMatrixElement, but add more diagnostics
function CheckYMatrixforZeroes(ckt: TDSSCircuit): String;

var
    i: Longword;
    c: Complex;
    hY: NativeUInt;
    sCol: Longword;
    nIslands, iCount, iFirst, p: Longword;
    Cliques: array of Longword;
    nodeInfo: TNodeBus;
begin
    Result := '';
    hY := ckt.Solution.hY;
    for i := 1 to ckt.NumNodes do
    begin
        GetMatrixElement(hY, i, i, @c);
        if Cabs(C) = 0.0 then
        begin
            nodeInfo := ckt.MapNodeToBus[i];
            Result := Result + Format(_('%sZero diagonal for bus %s, node %d'), [CRLF, ckt.BusList.NameOfIndex(nodeInfo.BusRef), nodeInfo.NodeNum]);
        end;
    end;

    // new diagnostics
    GetSingularCol(hY, @sCol); // returns a 1-based node number
    if sCol > 0 then
    begin
        nodeInfo := ckt.MapNodeToBus[sCol];
        Result := Result + Format(_('%sMatrix singularity at bus %s, node %d'), [CRLF, ckt.BusList.NameOfIndex(nodeInfo.BusRef), sCol]);
    end;

    SetLength(Cliques, ckt.NumNodes);
    nIslands := FindIslands(hY, ckt.NumNodes, @Cliques[0]);
    if nIslands <= 0 then
        Exit;

    Result := Result + Format(_('%sFound %d electrical islands:'), [CRLF, nIslands]);
    for i := 1 to nIslands do
    begin
        iCount := 0;
        iFirst := 0;
        for p := 0 to ckt.NumNodes - 1 do
        begin
            if Cliques[p] = i then
            begin
                Inc(iCount, 1);
                if iFirst = 0 then
                    iFirst := p + 1;
            end;
        end;
        nodeInfo := ckt.MapNodeToBus[iFirst];
        Result := Result + CRLF + Format(_('  #%d has %d nodes, including bus %s (node %d)'), [i, iCount, ckt.BusList.NameOfIndex(nodeInfo.BusRef), iFirst]);
    end;
end;


end.
