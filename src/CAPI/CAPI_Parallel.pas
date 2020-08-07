unit CAPI_Parallel;

{$inline on}

interface

uses
    CAPI_Utils;

function Parallel_Get_NumCPUs(): Integer; CDECL;
function Parallel_Get_NumCores(): Integer; CDECL;
function Parallel_Get_ActiveActor(): Integer; CDECL;
procedure Parallel_Set_ActiveActor(Value: Integer); CDECL;
procedure Parallel_CreateActor(); CDECL;
function Parallel_Get_ActorCPU(): Integer; CDECL;
procedure Parallel_Set_ActorCPU(Value: Integer); CDECL;
function Parallel_Get_NumOfActors(): Integer; CDECL;
procedure Parallel_Wait(); CDECL;
procedure Parallel_Get_ActorProgress(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Parallel_Get_ActorProgress_GR(); CDECL;
procedure Parallel_Get_ActorStatus(var ResultPtr: PByte; ResultCount: PAPISize); CDECL;
procedure Parallel_Get_ActorStatus_GR(); CDECL;
function Parallel_Get_ActiveParallel(): Boolean; CDECL;
procedure Parallel_Set_ActiveParallel(Value: Boolean); CDECL;
function Parallel_Get_ConcatenateReports(): Boolean; CDECL;
procedure Parallel_Set_ConcatenateReports(Value: Boolean); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    SysUtils,
    solution,
    CktElement,
    ParserDel,
    KLUSolve,
    Classes;

function Parallel_Get_NumCPUs(): Integer; CDECL;
begin
    Result := CPU_Cores;
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumCores(): Integer; CDECL;
begin
    Result := round(CPU_Cores / 2);
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActiveActor(): Integer; CDECL;
begin
    Result := ActiveActor;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActiveActor(Value: Integer); CDECL;
begin
    if Value <= NumOfActors then
        ActiveActor := Value
    else
        DoSimpleMsg('The actor does not exists', 7002);
end;
//------------------------------------------------------------------------------
procedure Parallel_CreateActor(); CDECL;
begin
    New_Actor_Slot();
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActorCPU(): Integer; CDECL;
begin
    Result := ActorCPU[ActiveActor];
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActorCPU(Value: Integer); CDECL;
begin
    if Value < CPU_Cores then
    begin
        ActorCPU[ActiveActor] := value;
        if ActorHandle[ActiveActor] <> nil then
            ActorHandle[ActiveActor].CPU := ActorCPU[ActiveActor];
    end
    else DoSimpleMsg('The CPU does not exist', 7004);
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumOfActors(): Integer; CDECL;
begin
    Result := NumOfActors;
end;
//------------------------------------------------------------------------------
procedure Parallel_Wait(); CDECL;
var
    i: Integer;
begin
    if Parallel_enabled then
        Wait4Actors(0);
end;
//------------------------------------------------------------------------------
procedure Parallel_Get_ActorProgress(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray;
    idx: Integer;
begin
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, NumOfActors);
    for idx := 1 to NumOfActors do
    begin
        Result[idx - 1] := ActorPctProgress[idx];
    end;
end;

procedure Parallel_Get_ActorProgress_GR(); CDECL;
// Same as Parallel_Get_ActorProgress but uses global result (GR) pointers
begin
    Parallel_Get_ActorProgress(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
procedure Parallel_Get_ActorStatus(var ResultPtr: PByte; ResultCount: PAPISize); CDECL;
var
    Result: PByteArray;
    idx: Integer;
begin
    Result := DSS_RecreateArray_PByte(ResultPtr, ResultCount, NumOfActors);
    for idx := 1 to NumOfActors do
    begin
        if ActorHandle[idx].Is_Busy then
            Result[idx - 1] := 0
        else
            Result[idx - 1] := 1;
    end;
end;

procedure Parallel_Get_ActorStatus_GR(); CDECL;
// Same as Parallel_Get_ActorStatus but uses global result (GR) pointers
begin
    Parallel_Get_ActorStatus(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
function Parallel_Get_ActiveParallel(): Integer; CDECL;
begin
    Result := Parallel_enabled;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActiveParallel(Value: Boolean); CDECL;
begin
    Parallel_enabled := Value;
end;
//------------------------------------------------------------------------------
function Parallel_Get_ConcatenateReports(): Boolean; CDECL;
begin
    Result := ConcatenateReports;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ConcatenateReports(Value: Boolean); CDECL;
begin
    ConcatenateReports := Value;
end;
//------------------------------------------------------------------------------
end.
