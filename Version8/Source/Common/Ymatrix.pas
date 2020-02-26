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
    SysUtils,
    windows,
    DSSClass,
    DSSObject;


{Options for building Y matrix}
const
    SERIESONLY = 1;
    WHOLEMATRIX = 2;

type
    EEsolv32Problem = class(Exception);

procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Boolean; ActorID: Integer);

procedure ResetSparseMatrix(var hY: NativeUint; size: Integer; ActorID: Integer);
procedure InitializeNodeVbase(ActorID: Integer);

function CheckYMatrixforZeroes(ActorID: Integer): String;

implementation

uses
    DSSGlobals,
    Circuit,
    CktElement,
    Utilities,
    KLUSolve;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ReCalcAllYPrims(ActorID: Integer);

var
    pElem: TDSSCktElement;

begin

    with ActiveCircuit[ActorID] do
    begin
        if LogEvents then
            LogThisEvent('Recalc All Yprims', ActorID);
        pElem := CktElements.First;
        while pElem <> NIL do
        begin
            pElem.CalcYPrim(ActorID);
            pElem := CktElements.Next;
        end;
    end;

end;

//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ReCalcInvalidYPrims(ActorID: Integer);
{Recalc YPrims only for those circuit elements that have had changes since last
 solution}
var
    pElem: TDSSCktElement;

begin

    with ActiveCircuit[ActorID] do
    begin
        if LogEvents then
            LogThisEvent('Recalc Invalid Yprims', ActorID);
        pElem := CktElements.First;
        while pElem <> NIL do
        begin
            with pElem do
                if YprimInvalid[ActorID] then
                    CalcYPrim(ActorID);
            pElem := CktElements.Next;
        end;
    end;

end;


//= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =
procedure ResetSparseMatrix(var hY: NativeUint; size: Integer; ActorID: Integer);


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


procedure InitializeNodeVbase(ActorID: Integer);

var
    i: Integer;

begin

    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin
        for i := 1 to NumNodes do
            with MapNodeToBus^[i] do
            begin
                NodeVbase^[i] := Buses^[BusRef].kvbase * 1000.0;
            end;
        VoltageBaseChanged := FALSE;
    end;
end;

procedure BuildYMatrix(BuildOption: Integer; AllocateVI: Boolean; ActorID: Integer);

{Builds designated Y matrix for system and allocates solution arrays}

var
    YMatrixsize: Integer;
//   CmatArray    :pComplexArray;   Replaced with a global array for thread safe operation
    pElem: TDSSCktElement;

   //{****} FTrace: TextFile;


begin
  //{****} AssignFile(Ftrace, 'YmatrixTrace.txt');
  //{****} Rewrite(FTrace);
    ActiveYPrim[ActorID] := NIL;  //Replaces the previous local declaration CmatArray := Nil; for thread safe
   // new function to log KLUSolve.DLL function calls
   // SetLogFile ('KLU_Log.txt', 1);
    with ActiveCircuit[ActorID], ActiveCircuit[ActorID].Solution do
    begin

        if PreserveNodeVoltages then
            UpdateVBus(ActorID); // Update voltage values stored with Bus object

     // the following re counts the number of buses and resets meter zones and feeders
     // If radial but systemNodeMap not set then init for radial got skipped due to script sequence
        if (BusNameRedefined) then
            ReProcessBusDefs(ActorID);      // This changes the node references into the system Y matrix!!

        YMatrixSize := NumNodes;
        case BuildOption of
            WHOLEMATRIX:
            begin
                ResetSparseMatrix(hYsystem, YMatrixSize, ActorID);
                hY := hYsystem;
            end;
            SERIESONLY:
            begin
                ResetSparseMatrix(hYseries, YMatrixSize, ActorID);
                hY := hYSeries;
            end;
        end;
     // tune up the Yprims if necessary
        if (FrequencyChanged) then
            ReCalcAllYPrims(ActorID)
        else
            ReCalcInvalidYPrims(ActorID);

        if SolutionAbort then
        begin
            DoSimpleMsg('Y matrix build aborted due to error in primitive Y calculations.', 11001);
            Exit;  // Some problem occured building Yprims
        end;


        FrequencyChanged := FALSE;

        if LogEvents then
            case BuildOption of
                WHOLEMATRIX:
                    LogThisEvent('Building Whole Y Matrix', ActorID);
                SERIESONLY:
                    LogThisEvent('Building Series Y Matrix', ActorID);
            end;
          // Add in Yprims for all devices
        pElem := ActiveCircuit[ActorID].CktElements.First;
        while pElem <> NIL do
        begin
            with pElem do
                if (Enabled) then
                begin          // Add stuff only if enabled
                    case BuildOption of
                        WHOLEMATRIX:
                            ActiveYPrim[ActorID] := GetYPrimValues(ALL_YPRIM);
                        SERIESONLY:
                            ActiveYPrim[ActorID] := GetYPrimValues(SERIES)
                    end;
           // new function adding primitive Y matrix to KLU system Y matrix
                    if ActiveYPrim[ActorID] <> NIL then
                        if AddPrimitiveMatrix(hY, Yorder, @NodeRef[1], @ActiveYPrim[ActorID][1]) < 1 then
                            raise EEsolv32Problem.Create('Node index out of range adding to System Y Matrix')
                end;   // If Enabled
            pElem := ActiveCircuit[ActorID].CktElements.Next;
        end;
     //{****} CloseFile(Ftrace);
     //{****} FireOffEditor(  'YmatrixTrace.txt');

     // Allocate voltage and current vectors if requested
        if AllocateVI then
        begin
            if LogEvents then
                LogThisEvent('ReAllocating Solution Arrays', ActorID);
            ReAllocMem(NodeV, SizeOf(NodeV^[1]) * (NumNodes + 1)); // Allocate System Voltage array - allow for zero element
            NodeV^[0] := CZERO;
            ReAllocMem(Currents, SizeOf(Currents^[1]) * (NumNodes + 1)); // Allocate System current array
            ReAllocMem(AuxCurrents, SizeOf(AuxCurrents^[1]) * (NumNodes + 1)); // Allocate System current array

         {A-Diakoptics vectors memory allocation}
            ReAllocMem(Node_dV, SizeOf(Node_dV^[1]) * (NumNodes + 1)); // Allocate the partial solution voltage
            ReAllocMem(Ic_Local, SizeOf(Ic_Local^[1]) * (NumNodes + 1)); // Allocate the Complementary currents

            if (VMagSaved <> NIL) then
                ReallocMem(VMagSaved, 0);
            if (ErrorSaved <> NIL) then
                ReallocMem(ErrorSaved, 0);
            if (NodeVBase <> NIL) then
                ReallocMem(NodeVBase, 0);
            if (NodeYii <> NIL) then
                ReallocMem(NodeYii, 0);             {by Dahei -> UCF}
            VMagSaved := AllocMem(Sizeof(VMagSaved^[1]) * NumNodes);  // zero fill
            ErrorSaved := AllocMem(Sizeof(ErrorSaved^[1]) * NumNodes);  // zero fill
            NodeVBase := AllocMem(Sizeof(NodeVBase^[1]) * NumNodes);  // zero fill
            NodeYii := AllocMem(Sizeof(NodeYii^[1]) * (NumNodes + 1));  // zero fill //Bii  {by Dahei -> UCF}
            NodeYiiEmpty := TRUE;                                                          {by Dahei -> UCF}
            InitializeNodeVbase(ActorID);


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
function CheckYMatrixforZeroes(ActorID: Integer): String;

var
    i: Longword;
    c: Complex;
    hY: NativeUInt;
    sCol: Longword;
    nIslands, iCount, iFirst, p: Longword;
    Cliques: array of Longword;
begin

    Result := '';
    with ActiveCircuit[ActorID] do
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
