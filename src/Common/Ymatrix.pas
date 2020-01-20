unit Ymatrix;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2019, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

{
   Unit to manage System Y matrix

   6-11-00  Created from Solution.Pas
}

interface

uses
    uComplex,
    ucMatrix,
    SysUtils,
    DSSClass;


{Options for building Y matrix}
const
    SERIESONLY = 1;
    WHOLEMATRIX = 2;

type
    EEsolv32Problem = class(Exception);


procedure BuildYMatrix(DSS: TDSSContext; BuildOption: Integer; AllocateVI: Boolean);
procedure ResetSparseMatrix(var hY: NativeUint; size: Integer);
procedure InitializeNodeVbase(DSS: TDSSContext);

function CheckYMatrixforZeroes(DSS: TDSSContext): String;

implementation

uses
    DSSGlobals,
    Circuit,
    CktElement,
    Utilities,
    KLUSolve,
    Solution,
    DSSClassDefs,
    GUtil,
    GSet,
    DSSHelper;


type 
    TCoordLess = TLess<QWord>;
    TCoordSet = TSet<QWord, TCoordLess>;
    TNodeLess = TLess<Integer>;
    TNodeSet = TSet<Integer, TNodeLess>;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ReCalcAllYPrims(Ckt: TDSSCircuit);

var
    pElem: TDSSCktElement;

begin
    with Ckt do
    begin
        if LogEvents then
            LogThisEvent(Ckt.DSS, 'Recalc All Yprims');
        pElem := CktElements.First;
        while pElem <> NIL do
        begin
            pElem.CalcYPrim;
            pElem := CktElements.Next;
        end;
    end;
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ReCalcInvalidYPrims(Ckt: TDSSCircuit);
{Recalc YPrims only for those circuit elements that have had changes since last solution}
var
    pElem: TDSSCktElement;
begin
    with Ckt do
    begin
        if LogEvents then
            LogThisEvent(Ckt.DSS, 'Recalc Invalid Yprims');

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        pElem := IncrCktElements.First;
        while pElem <> NIL do
        begin
            with pElem do
                if YprimInvalid then
                begin
                    CalcYPrim;
                end;
            pElem := IncrCktElements.Next;
        end;
{$ENDIF}
        pElem := CktElements.First;
        while pElem <> NIL do
        begin
            with pElem do
                if YprimInvalid {or ((DSSObjType and CLASSMASK) = LOAD_ELEMENT)} then
                begin
                    CalcYPrim;
                end;
            pElem := CktElements.Next;
        end;
    end;
end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ResetSparseMatrix(var hY: NativeUint; size: Integer);
begin
    if hY <> 0 then
    begin
        if DeleteSparseSet(hY) < 1  {Get rid of existing one beFore making a new one} then
            raise EEsolv32Problem.Create('Error Deleting System Y Matrix in ResetSparseMatrix. Problem with Sparse matrix solver.');

        hY := 0;
    end;

    // Make a new sparse set
    hY := NewSparseSet(Size);
    if hY < 1 then
    begin
        raise EEsolv32Problem.Create('Error Creating System Y Matrix. Problem WITH Sparse matrix solver.');
    end;
end;


procedure InitializeNodeVbase(DSS: TDSSContext);
var
    i: Integer;
begin
    with DSS.ActiveCircuit, Solution do
    begin
        for i := 1 to NumNodes do
            with MapNodeToBus^[i] do
            begin
                NodeVbase^[i] := Buses^[BusRef].kvbase * 1000.0;
            end;
        VoltageBaseChanged := FALSE;
    end;
end;

{$IFDEF DSS_CAPI_INCREMENTAL_Y}
function UpdateYMatrix(Ckt: TDSSCircuit; BuildOption: Integer; AllocateVI: Boolean): Boolean;
var
    IncrYprim: TCMatrix;
    Norder: Integer;
    pElem: TDSSCktElement;
    CmatArray: pComplexArray;
    error, n: Integer;
    changedElements: TCoordSet;
    changedNodes: TNodeSet;
    coordIt: TCoordSet.TIterator;
    
    i, j, nref, inode, jnode: Integer;
    abortIncremental: Boolean;
    val: Complex;
begin
    changedElements := TCoordSet.Create;
    changedNodes := TNodeSet.Create;
    Result := False;
    CmatArray := NIL;
    IncrYprim := NIL;
    abortIncremental := False;

    // Incremental Y update, only valid for BuildOption = WHOLEMATRIX.
    pElem := Ckt.IncrCktElements.First;
    while pElem <> NIL do with pElem do
    begin
        if (Enabled and (Yprim = NIL)) then
        begin
            abortIncremental := True;
            break;
        end;

        if (Enabled and (Yprim <> NIL)) then
        begin
            if IncrYprim <> NIL then
            begin
                IncrYprim.Free;
                IncrYprim := NIL;
            end;
            
            IncrYprim := TCmatrix.CreateMatrix(Yprim.order);
            IncrYprim.CopyFrom(Yprim);
            IncrYprim.Negate();
            
            CalcYPrim;
            

            if (Yprim = NIL) or (IncrYprim.order <> Yprim.order) then
            begin
                abortIncremental := True;
                break;
            end;
            
            IncrYprim.AddFrom(YPrim);
            for i := 1 to Yprim.order do
            begin
                inode := NodeRef[i];
                if inode = 0 then continue;
                for j := 1 to Yprim.order do
                begin
                    jnode := NodeRef[j];
                    if jnode = 0 then continue;
                    
                    val := IncrYprim.GetElement(i, j);
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
        pElem := Ckt.IncrCktElements.Next;
    end;

    if IncrYprim <> NIL then
    begin
        IncrYprim.Free;
        IncrYprim := NIL;
    end;

    if not abortIncremental then
    begin
        coordIt := changedElements.Min;
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

    pElem := Ckt.CktElements.First;
    while (not abortIncremental) and (pElem <> NIL) do with pElem do
    begin
        if (not Enabled) or (Yprim = NIL) then
        begin
            pElem := Ckt.CktElements.Next;
            continue;
        end;

        for i := 1 to Yprim.order do
        begin
            inode := NodeRef[i];
            if inode = 0 then continue;
            if changedNodes.Find(inode) = NIL then
                // nothing changed for node "inode", we can skip it completely
                continue;

            for j := 1 to Yprim.order do
            begin
                jnode := NodeRef[j];
                if jnode = 0 then continue;

                if (changedElements.Find((QWord(inode) shl 32) or (QWord(jnode))) = nil) then
                    continue;

                val := Yprim.GetElement(i, j);
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

        pElem := Ckt.CktElements.Next;
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

{Builds designated Y matrix for system and allocates solution arrays}

var
    YMatrixsize: Integer;
    CmatArray: pComplexArray;
    pElem: TDSSCktElement;
    c: Complex;
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
    Incremental: Boolean;
{$ENDIF}
   //{****} FTrace: TextFile;
begin
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
    Incremental := False;
{$ENDIF}
  //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
  //{****} Rewrite(FTrace);
    CmatArray := NIL;
   // new function to log KLUSolve.DLL function calls
   // SetLogFile ('KLU_Log.txt', 1);
    with DSS.ActiveCircuit, Solution do
    begin

        if PreserveNodeVoltages then
            UpdateVBus; // Update voltage values stored with Bus object

     // the following re counts the number of buses and resets meter zones and feeders
     // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
        if (BusNameRedefined) then
            ReProcessBusDefs;      // This changes the node references into the system Y matrix!!

        YMatrixSize := NumNodes;

        case BuildOption of
            WHOLEMATRIX:
            begin
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
                Incremental := (Solution.SolverOptions <> ord(TSolverOptions.ReuseNothing)) and 
                    (not SystemYChanged) and 
                    (IncrCktElements.ListSize <> 0) and 
                    (not AllocateVI) and 
                    (not FrequencyChanged);

                if not Incremental then
                begin
                    if IncrCktElements.ListSize <> 0 then
                        SystemYChanged := True;
{$ENDIF}
                    ResetSparseMatrix(hYsystem, YMatrixSize);
                    KLUSolve.SetOptions(hYsystem, SolverOptions);
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
                end;
{$ENDIF}
                hY := hYsystem;
            end;
            SERIESONLY:
            begin
                ResetSparseMatrix(hYseries, YMatrixSize);
                KLUSolve.SetOptions(hYsystem, SolverOptions);
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
            DoSimpleMsg(DSS, 'Y matrix build aborted due to error in primitive Y calculations.', 11001);
            Exit;  // Some problem occured building Yprims
        end;

        FrequencyChanged := FALSE;

        if LogEvents then
            case BuildOption of
                WHOLEMATRIX:
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
                    if Incremental then
                        LogThisEvent(DSS, 'Building Whole Y Matrix -- using incremental method')
                    else
{$ENDIF}
                        LogThisEvent(DSS, 'Building Whole Y Matrix');
                        
                SERIESONLY:
                    LogThisEvent(DSS, 'Building Series Y Matrix');
            end;
          // Add in Yprims for all devices
          
        // Full method, handles all elements
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        if not Incremental then
        begin
{$ENDIF}
            // Full method, handles all elements
            pElem := CktElements.First;
            while pElem <> NIL do
            begin
                with pElem do
                    if (Enabled) then
                    begin          // Add stuff only if enabled
                        case BuildOption of
                            WHOLEMATRIX:
                                CmatArray := GetYPrimValues(ALL_YPRIM);
                            SERIESONLY:
                                CmatArray := GetYPrimValues(SERIES)
                        end;
               // new function adding primitive Y matrix to KLU system Y matrix
                        if CMatArray <> NIL then
                            if AddPrimitiveMatrix(hY, Yorder, @NodeRef[1], @CMatArray[1]) < 1 then
                                raise EEsolv32Problem.Create('Node index out of range adding to System Y Matrix')
                    end;   // If Enabled
                pElem := CktElements.Next;
            end;
{$IFDEF DSS_CAPI_INCREMENTAL_Y}
        end // if not Incremental
        else
        begin // if Incremental 
            if not UpdateYMatrix(DSS.ActiveCircuit, BuildOption, AllocateVI) then
                Exit;
        end;
{$ENDIF}
       
     //{****} CloseFile(Ftrace);
     //{****} FireOffEditor(  'YmatrixTrace.txt');

     // Allocate voltage and current vectors if requested
        if AllocateVI then
        begin
            if LogEvents then
                LogThisEvent(DSS, 'ReAllocating Solution Arrays');
            ReAllocMem(NodeV, SizeOf(NodeV^[1]) * (NumNodes + 1)); // Allocate System Voltage array - allow for zero element
            NodeV^[0] := CZERO;
            ReAllocMem(Currents, SizeOf(Currents^[1]) * (NumNodes + 1)); // Allocate System current array
            ReAllocMem(AuxCurrents, SizeOf(AuxCurrents^[1]) * (NumNodes + 1)); // Allocate System current array
            if (VMagSaved <> NIL) then
                ReallocMem(VMagSaved, 0);
            if (ErrorSaved <> NIL) then
                ReallocMem(ErrorSaved, 0);
            if (NodeVBase <> NIL) then
                ReallocMem(NodeVBase, 0);
            VMagSaved := AllocMem(Sizeof(VMagSaved^[1]) * NumNodes);  // zero fill
            ErrorSaved := AllocMem(Sizeof(ErrorSaved^[1]) * NumNodes);  // zero fill
            NodeVBase := AllocMem(Sizeof(NodeVBase^[1]) * NumNodes);  // zero fill
            InitializeNodeVbase(DSS);
{$IFDEF DSS_CAPI_PM}
            {A-Diakoptics vectors memory allocation}
            ReAllocMem(Node_dV, SizeOf(Node_dV^[1]) * (NumNodes + 1)); // Allocate the partial solution voltage
            ReAllocMem(Ic_Local, SizeOf(Ic_Local^[1]) * (NumNodes + 1)); // Allocate the Complementary currents
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

//    GetMatrixElement(ActiveCircuit.Solution.hY, 5, 5, @c);
//    writeln('BuildYMatrix: (4, 4) = ', c.re, ' +j', c.im);
end;

// leave the call to GetMatrixElement, but add more diagnostics
function CheckYMatrixforZeroes(DSS: TDSSContext): String;

var
    i: Longword;
    c: Complex;
    hY: NativeUInt;
    sCol: Longword;
    nIslands, iCount, iFirst, p: Longword;
    Cliques: array of Longword;
begin

    Result := '';
    with DSS.ActiveCircuit do
    begin
        hY := Solution.hY;
        for i := 1 to Numnodes do
        begin
            GetMatrixElement(hY, i, i, @c);
            if Cabs(C) = 0.0 then
                with MapNodeToBus^[i] do
                begin
                    Result := Result + Format('%sZero diagonal for bus %s, node %d', [CRLF, BusList.Get(Busref), NodeNum]);
                end;
        end;

    // new diagnostics
        GetSingularCol(hY, @sCol); // returns a 1-based node number
        if sCol > 0 then
            with MapNodeToBus^[sCol] do
            begin
                Result := Result + Format('%sMatrix singularity at bus %s, node %d', [CRLF, BusList.Get(Busref), sCol]);
            end;

        SetLength(Cliques, NumNodes);
        nIslands := FindIslands(hY, NumNodes, @Cliques[0]);
        if nIslands > 1 then
        begin
            Result := Result + Format('%sFound %d electrical islands:', [CRLF, nIslands]);
            for i := 1 to nIslands do
            begin
                iCount := 0;
                iFirst := 0;
                for p := 0 to NumNodes - 1 do
                begin
                    if Cliques[p] = i then
                    begin
                        Inc(iCount, 1);
                        if iFirst = 0 then
                            iFirst := p + 1;
                    end;
                end;
                with MapNodeToBus^[iFirst] do
                begin
                    Result := Result + Format('%s  #%d has %d nodes, including bus %s (node %d)', [CRLF, i, iCount, BusList.Get(Busref), iFirst]);
                end;
            end;
        end;
    end;

end;


end.
