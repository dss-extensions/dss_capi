unit DParallel;

interface

uses
    Arraydef,
    UComplex,
    Solution;

function ParallelI(mode: Longint; arg: Longint): Longint; CDECL;
procedure ParallelV(mode: Longint; out arg: Variant); CDECL;

implementation

uses
    DSSGlobals,
    Executive,
    Dialogs,
    SysUtils,
    Variants,
    CktElement,
    ParserDel,
    KLUSolve,
    System.Classes;

function ParallelI(mode: Longint; arg: Longint): Longint; CDECL;
var
    i: Integer;

begin
    Result := 0;             // Default return value
    case mode of
        0:
        begin  // Parallel.NumCPUs Read
            Result := CPU_Cores;
        end;
        1:
        begin  // Parallel.NumCores Read
            Result := round(CPU_Cores / 2);
        end;
        2:
        begin  // Parallel.ActiveActor Read
            Result := ActiveActor;
        end;
        3:
        begin  // Parallel.ActiveActor Write
            if arg <= NumOfActors then
                ActiveActor := arg
            else
                DoSimpleMsg('The actor does not exists', 7002);
        end;
        4:
        begin  // Parallel.CreateActor Write
            New_Actor_Slot();
        end;
        5:
        begin  // Parallel.ActorCPU Read
            Result := ActorCPU[ActiveActor];
        end;
        6:
        begin  // Parallel.ActorCPU Write
            if arg < CPU_Cores then
            begin
                ActorCPU[ActiveActor] := arg;
                if ActorHandle[ActiveActor] <> NIL then
                    ActorHandle[ActiveActor].CPU := ActorCPU[ActiveActor];
            end
            else
                DoSimpleMsg('The CPU does not exists', 7004);
        end;
        7:
        begin  // Parallel.NumActors Read
            Result := NumOfActors;
        end;
        8:
        begin  // Parallel.Wait
            if Parallel_enabled then
                Wait4Actors(0);
        end;
        9:
        begin  // Parallel.ActiveParallel Read
            if Parallel_enabled then
                Result := 1
            else
                Result := 0;
        end;
        10:
        begin  // Parallel.ActiveParallel Write
            if arg <> 0 then
                Parallel_enabled := TRUE
            else
                Parallel_enabled := FALSE;
        end;
        11:
        begin  // Parallel.ConcatenateReports Read
            if ConcatenateReports then
                Result := 1
            else
                Result := 0;
        end;
        12:
        begin  // Parallel.ConcatenateReports Write
            if arg <> 0 then
                ConcatenateReports := TRUE
            else
                ConcatenateReports := FALSE;
        end
    else
        Result := -1;
    end;
end;


procedure ParallelV(mode: Longint; out arg: Variant); CDECL;
var
    i: Integer;

begin
    arg := VarArrayCreate([1, NumOfActors], varInteger);
    ;             // Default return value
    case mode of
        0:
        begin  // Parallel.ActorProgress Read
            for i := 1 to NumOfActors do
            begin
                arg[i] := ActorPctProgress[i];
            end;
        end;
        1:
        begin  // Parallel.AxtorState Read
            for i := 1 to NumOfActors do
            begin
                arg[i] := ActorStatus[i];
            end;
        end
    else
        arg[1] := -1;
    end;

end;

end.
