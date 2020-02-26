unit DYMatrix;

interface

uses
    Arraydef,
    UComplex,
    Solution;

function InitAndGetYparams(var hY: NativeUInt; var nBus, nNZ: Longword): Longword; CDECL;
procedure GetCompressedYMatrix(hY: NativeUInt; nBus, nNz: Longword; var ColPtr, RowIdx: pIntegerArray; var cVals: pComplexArray); CDECL;
//01202016
procedure ZeroInjCurr; CDECL;
procedure GetSourceInjCurrents; CDECL;
procedure GetPCInjCurr; CDECL;
function SystemYChanged(mode, arg: Longint): Longint; CDECL;
procedure BuildYMatrixD(BuildOps, AllocateVI: Longint); CDECL;
function UseAuxCurrents(mode, arg: Longint): Longint; CDECL;
procedure AddInAuxCurrents(SType: Integer); CDECL;
procedure getIpointer(var IvectorPtr: pNodeVarray); CDECL;
procedure getVpointer(var VvectorPtr: pNodeVarray); CDECL;
function SolveSystem(var NodeV: pNodeVarray): Integer; CDECL;


implementation

uses
    DSSGlobals,
    Ymatrix,
    KLUSolve;

var {Global variables in this Module}
    Yhandle: NativeUInt;
    NumNZ, NumBuses: Longword;
    YColumns,
    YRows: pIntegerArray;
    YValues: pComplexArray;


function InitAndGetYparams(var hY: NativeUInt; var nBus, nNZ: Longword): Longword; CDECL;

// Call this first

// Save a copy of these in permanent heap memory here before returning

begin
    Result := 0;    // indicates error
    if ActiveCircuit[ActiveActor] = NIL then
        Exit;
    Yhandle := ActiveCircuit[ActiveActor].Solution.hY;
    if Yhandle <= 0 then
    begin
        DoSimpleMsg('Y Matrix not Built.', 222);
        Exit;
    end;

    hY := Yhandle;

    FactorSparseMatrix(hY);
    GetNNZ(hY, @NumNz);
    GetSize(hY, @NumBuses);

    nBus := NumBuses;
    nNZ := NumNZ;

    Result := 1;
end;


procedure GetCompressedYMatrix(hY: NativeUInt; nBus, nNz: Longword; var ColPtr, RowIdx: pIntegerArray; var cVals: pComplexArray); CDECL;

{Returns Pointers to column and row and matrix values}

{Call InitAndGetYparams first to factor the sparse matrix ...}

begin

 // Allocate space on the heap and put the values there
    ReAllocmem(YColumns, sizeof(YColumns^[1]) * (nBus + 1));
    ReAllocmem(YRows, sizeof(YRows^[1]) * nNZ);
    ReAllocmem(YValues, sizeof(YValues^[1]) * nNZ);
     // Fill in the memory
    GetCompressedMatrix(hY, nBus + 1, nNZ, @YColumns^[1], @YRows^[1], @YValues^[1]);

     // Set the pointers in the calling program to the heap variables
    ColPtr := YColumns;
    RowIdx := YRows;
    cVals := YValues;
end;

procedure ZeroInjCurr; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.ZeroInjCurr(ActiveActor);
end;

procedure GetSourceInjCurrents; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.GetSourceInjCurrents(ActiveActor);
end;

procedure GetPCInjCurr; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        ActiveCircuit[ActiveActor].Solution.GetPCInjCurr(ActiveActor);
end;

function SystemYChanged(mode, arg: Longint): Longint; CDECL;
begin
    Result := 0;
    case mode of
        0:
            if ActiveCircuit[ActiveActor].Solution.SystemYChanged then
                Result := 1;  // Read
        1:
        begin                                                      // Write
            if arg = 1 then
                ActiveCircuit[ActiveActor].Solution.SystemYChanged := TRUE
            else
                ActiveCircuit[ActiveActor].Solution.SystemYChanged := FALSE;
        end;
    end;
end;

procedure BuildYMatrixD(BuildOps, AllocateVI: Longint); CDECL;
var
    AllocateV: Boolean;
begin
    AllocateV := FALSE;
    if AllocateVI = 1 then
        AllocateV := TRUE;
    BuildYMatrix(BuildOps, AllocateV, ActiveActor);
end;

function UseAuxCurrents(mode, arg: Longint): Longint; CDECL;
begin
    Result := 0;
    case mode of
        0:
            if ActiveCircuit[ActiveActor].Solution.UseAuxCurrents then
                Result := 1;  // Read
        1:
        begin                                                      // Write
            if arg = 1 then
                ActiveCircuit[ActiveActor].Solution.UseAuxCurrents := TRUE
            else
                ActiveCircuit[ActiveActor].Solution.UseAuxCurrents := FALSE;
        end;
    end;
end;

procedure AddInAuxCurrents(SType: Integer); CDECL;
begin
    ActiveCircuit[ActiveActor].Solution.AddInAuxCurrents(SType, ActiveActor);
end;

procedure getIpointer(var IvectorPtr: pNodeVarray); CDECL;
begin
    IVectorPtr := ActiveCircuit[ActiveActor].Solution.Currents;
end;

procedure getVpointer(var VvectorPtr: pNodeVarray); CDECL;
begin
    VVectorPtr := ActiveCircuit[ActiveActor].Solution.NodeV;
end;

function SolveSystem(var NodeV: pNodeVarray): Integer; CDECL;
begin
    Result := ActiveCircuit[ActiveActor].Solution.SolveSystem(NodeV, ActiveActor);
end;

//---------------------------------------------------------------------------------

initialization

// Initialize so Reallocmem will work reliably
    Ycolumns := NIL;
    YRows := NIL;
    YValues := NIL;

finalization

// Be a good citizen and clean up
    ReAllocmem(YColumns, 0);
    ReAllocmem(YRows, 0);
    ReAllocmem(YValues, 0);

end.
