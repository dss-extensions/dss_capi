unit Ymatrix;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
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
    SysUtils;


{Options for building Y matrix}
const
    SERIESONLY = 1;
    WHOLEMATRIX = 2;

type
    EEsolv32Problem = class(Exception);


procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Boolean);
procedure ResetSparseMatrix(var hY: NativeUint; size: Integer);
procedure InitializeNodeVbase;

function CheckYMatrixforZeroes: String;

implementation

uses
    DSSGlobals,
    Circuit,
    CktElement,
    Utilities,
    KLUSolve,
    DSSClassDefs,
    GUtil,
    GSet,
    DSSClass,
    DSSHelper;


type 
    TCoordLess = TLess<QWord>;
    TCoordSet = TSet<QWord, TCoordLess>;
    TNodeLess = TLess<Integer>;
    TNodeSet = TSet<Integer, TNodeLess>;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ReCalcAllYPrims;

var
    pElem: TDSSCktElement;

begin

//    writeln('!!!Recalc ALL Yprims');
    
    with ActiveCircuit do
    begin
        if LogEvents then
            LogThisEvent('Recalc All Yprims');
        pElem := CktElements.First;
        while pElem <> NIL do
        begin
            pElem.CalcYPrim;
            pElem := CktElements.Next;
        end;
    end;

//    writeln('<<<Recalc ALL Yprims');
end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ReCalcInvalidYPrims;
{Recalc YPrims only for those circuit elements that have had changes since last
 solution}
var
    pElem: TDSSCktElement;

begin
//    writeln('!!!Recalc Invalid Yprims');

    with ActiveCircuit do
    begin
        if LogEvents then
            LogThisEvent('Recalc Invalid Yprims');
            
        pElem := IncrCktElements.First;
        while pElem <> NIL do
        begin
            with pElem do
                if YprimInvalid then
                begin
                    //WriteLn('ReCalcInvalidYPrims: CalcYPrim for ', Name, ' (incremental)');
                    CalcYPrim;
                end;
            pElem := IncrCktElements.Next;
        end;

        pElem := CktElements.First;
        while pElem <> NIL do
        begin
            with pElem do
                if YprimInvalid {or ((DSSObjType and CLASSMASK) = LOAD_ELEMENT)} then
                begin
                    // WriteLn('ReCalcInvalidYPrims: CalcYPrim for ', Name, ' (normal)');
                    CalcYPrim;
                end;
            pElem := CktElements.Next;
        end;
    end;

//    writeln('<<< Recalc Invalid Yprims');
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
    begin   // Raise and exception
        raise EEsolv32Problem.Create('Error Creating System Y Matrix. Problem WITH Sparse matrix solver.');
    end;
end;


procedure InitializeNodeVbase;

var
    i: Integer;

begin

    with ActiveCircuit, Solution do
    begin
        for i := 1 to NumNodes do
            with MapNodeToBus^[i] do
            begin
                NodeVbase^[i] := Buses^[BusRef].kvbase * 1000.0;
            end;
        VoltageBaseChanged := FALSE;
    end;
end;

function UpdateYMatrix(BuildOption: Integer; AllocateVI: Boolean): Boolean;
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
    //skip: Boolean;
    val: Complex;
begin
    changedElements := TCoordSet.Create; //TODO: changedElements.Free;
    changedNodes := TNodeSet.Create;
    Result := False;
    CmatArray := NIL;
    IncrYprim := NIL;
    
    // Incremental Y update, only valid for BuildOption = WHOLEMATRIX.
    //writeln('IncrCktElements is null? ', IncrCktElements = nil);
    //writeln('Number of incremental elements: ', IncrCktElements.ListSize);
    
//    writeln('Analyzing incremental elements...');
    pElem := ActiveCircuit.IncrCktElements.First;
    while pElem <> NIL do with pElem do
    begin
        if (Enabled and (Yprim = NIL)) then 
        begin
            writeln('TODO: ABORT');
        end;
    
        if (Enabled and (Yprim <> NIL)) then 
        begin
//            writeln('> Incremental update -- handling ', pElem.ClassName, '.', pElem.Name);
            //writeln('CalcYPrim');
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
                writeln('TODO: ABORT -- Orders differ?', (IncrYprim.order <> Yprim.order));
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
                        changedElements.Insert((QWord(inode) shl 32) or QWord(jnode));
                        //writeln('!!!!', inode, '   ', jnode);
                    end;
                end;
            end;
                
//                // Retry with the full matrix instead
//                ActiveCircuit.IncrCktElements.Clear;
//                ActiveCircuit.Solution.SystemYChanged := True;
//                writeln('ERROR: Aborting incremental update, running normal method (transformer, branch 2). ', 'CMatArray IS NIL');
//                BuildYMatrix(BuildOption, AllocateVI);
//                Exit;
        end;
        pElem := ActiveCircuit.IncrCktElements.Next;
    end;
    
    if IncrYprim <> NIL then
    begin
        IncrYprim.Free;
        IncrYprim := NIL;
    end;
    
//    writeln('Changed nodes:', changedNodes.Size);
    coordIt := changedElements.Min;
    repeat
        // writeln('>Zeroising row and column for ', (coordIt.Data shr 32), ',', coordIt.Data and $FFFFFFFF);
        //TODO: zeroise only the exact elements affected to make it faster
        ZeroiseMatrixElement(ActiveCircuit.Solution.hYsystem, (coordIt.Data shr 32), coordIt.Data and $FFFFFFFF);
    until not coordIt.Next();
    

//    writeln('Checking all elements');
    
    pElem := ActiveCircuit.CktElements.First;
    while pElem <> NIL do with pElem do
    begin
        if (not Enabled) or (Yprim = NIL) then
        begin
            pElem := ActiveCircuit.CktElements.Next;
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
                
                //writeln('IncrementMatrixElement: (', inode, ',', jnode, '): ', val.re, ', ', val.im);
                IncrementMatrixElement(ActiveCircuit.Solution.hYsystem, inode, jnode, val.re, val.im);
            end;
        end;
        pElem := ActiveCircuit.CktElements.Next;
    end;

    ActiveCircuit.IncrCktElements.Clear;
    Result := True;
//    writeln('Incremental update finished.');
//    writeln();
end;

procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Boolean);

{Builds designated Y matrix for system and allocates solution arrays}

var
    YMatrixsize: Integer;
    CmatArray: pComplexArray;
    pElem: TDSSCktElement;
    Incremental: Boolean;
    c: Complex;
   //{****} FTrace: TextFile;
begin
    Incremental := False;
  //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
  //{****} Rewrite(FTrace);
    CmatArray := NIL;
   // new function to log KLUSolve.DLL function calls
   // SetLogFile ('KLU_Log.txt', 1);
    with ActiveCircuit, ActiveCircuit.Solution do
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
                Incremental := (not SystemYChanged) and (IncrCktElements.ListSize <> 0) and (not AllocateVI) and (not FrequencyChanged);
                if not Incremental then
                begin
                    ResetSparseMatrix(hYsystem, YMatrixSize);
                end;
                hY := hYsystem;
            end;
            SERIESONLY:
            begin
                ResetSparseMatrix(hYseries, YMatrixSize);
                Incremental := False;
                hY := hYSeries;
            end;
        end;

     // tune up the Yprims if necessary
        if not Incremental then 
        begin
            if (FrequencyChanged) then
                ReCalcAllYPrims
            else if not Incremental then
                ReCalcInvalidYPrims;
        end;
        
        
        if DSSPrime.SolutionAbort then
        begin
            DoSimpleMsg('Y matrix build aborted due to error in primitive Y calculations.', 11001);
            Exit;  // Some problem occured building Yprims
        end;

        FrequencyChanged := FALSE;

        if LogEvents then
            case BuildOption of
                WHOLEMATRIX:
                    if Incremental then
                        LogThisEvent('Building Whole Y Matrix -- using incremental method')
                    else
                        LogThisEvent('Building Whole Y Matrix');
                        
                SERIESONLY:
                    LogThisEvent('Building Series Y Matrix');
            end;
          // Add in Yprims for all devices
          
        // Full method, handles all elements
        if not Incremental then
        begin
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
        end // if not Incremental
        else
        begin // if Incremental 
            if not UpdateYMatrix(BuildOption, AllocateVI) then
                Exit;
        end;
        
       
     //{****} CloseFile(Ftrace);
     //{****} FireOffEditor(  'YmatrixTrace.txt');

     // Allocate voltage and current vectors if requested
        if AllocateVI then
        begin
            if LogEvents then
                LogThisEvent('ReAllocating Solution Arrays');
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
            InitializeNodeVbase;

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
function CheckYMatrixforZeroes: String;

var
    i: Longword;
    c: Complex;
    hY: NativeUInt;
    sCol: Longword;
    nIslands, iCount, iFirst, p: Longword;
    Cliques: array of Longword;
begin

    Result := '';
    with ActiveCircuit do
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
