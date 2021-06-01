unit CAPI_Parallel;

interface

uses
    CAPI_Utils,
    CAPI_Types;

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
procedure Parallel_Get_ActorStatus(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
procedure Parallel_Get_ActorStatus_GR(); CDECL;
function Parallel_Get_ActiveParallel(): Integer; CDECL;
procedure Parallel_Set_ActiveParallel(Value: Integer); CDECL;
function Parallel_Get_ConcatenateReports(): Integer; CDECL;
procedure Parallel_Set_ConcatenateReports(Value: Integer); CDECL;

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
    Classes,
    DSSClass,
    DSSHelper;


{$IFDEF DSS_CAPI_PM}
function Parallel_Get_NumCPUs(): Integer; CDECL;
begin
    Result := CPU_Cores;
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumCores(): Integer; CDECL;
begin
    Result := round(CPU_Cores / 2); //TODO: fix
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActiveActor(): Integer; CDECL;
begin
    Result := DSSPrime.ActiveChildIndex;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActiveActor(Value: Integer); CDECL;
begin
    if (Value > 0) and (Value <= DSSPrime.NumOfActors) then
    begin
        DSSPrime.ActiveChildIndex := Value - 1;
        DSSPrime.ActiveChild := DSSPrime.Children[DSSPrime.ActiveChildIndex];
    end
    else
        DoSimpleMsg(DSSPrime, 'The actor does not exists', 7002);
end;
//------------------------------------------------------------------------------
procedure Parallel_CreateActor(); CDECL;
begin
    New_Actor_Slot(DSSPrime);
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActorCPU(): Integer; CDECL;
begin
    Result := DSSPrime.ActiveChild.CPU;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActorCPU(Value: Integer); CDECL;
begin
    if Value < CPU_Cores then
    begin
        DSSPrime.ActiveChild.CPU := value;
        if DSSPrime.ActiveChild.ActorThread <> nil then
            DSSPrime.ActiveChild.ActorThread.CPU := value;
    end
    else DoSimpleMsg(DSSPrime, 'The CPU does not exist', 7004);
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumOfActors(): Integer; CDECL;
begin
    Result := DSSPrime.NumOfActors;
end;
//------------------------------------------------------------------------------
procedure Parallel_Wait(); CDECL;
var
    i: Integer;
begin
    if DSSPrime.Parallel_enabled then
        Wait4Actors(DSSPrime, 0);
end;
//------------------------------------------------------------------------------
procedure Parallel_Get_ActorProgress(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray0;
    idx: Integer;
begin
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, DSSPrime.NumOfActors);
    for idx := 0 to High(DSSPrime.Children) do
    begin
        Result[idx] := DSSPrime.Children[idx].ActorPctProgress;
    end;
end;

procedure Parallel_Get_ActorProgress_GR(); CDECL;
// Same as Parallel_Get_ActorProgress but uses global result (GR) pointers
begin
    Parallel_Get_ActorProgress(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
procedure Parallel_Get_ActorStatus(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
var
    Result: PIntegerArray0;
    idx: Integer;
begin
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, DSSPrime.NumOfActors);
    for idx := 0 to High(DSSPrime.Children) do
    begin
        if DSSPrime.Children[idx].ActorThread.Is_Busy then
            Result[idx] := Ord(TActorStatus.Busy)
        else
            Result[idx] := Ord(TActorStatus.Idle);
    end;
end;

procedure Parallel_Get_ActorStatus_GR(); CDECL;
// Same as Parallel_Get_ActorStatus but uses global result (GR) pointers
begin
    Parallel_Get_ActorStatus(DSSPrime.GR_DataPtr_PInteger, @DSSPrime.GR_Counts_PInteger[0])
end;

//------------------------------------------------------------------------------
function Parallel_Get_ActiveParallel(): Integer; CDECL;
begin
    if DSSPrime.Parallel_enabled then
        Result := 1
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActiveParallel(Value: Integer); CDECL;
begin
    DSSPrime.Parallel_enabled := (Value = 1); //TODO
end;
//------------------------------------------------------------------------------
function Parallel_Get_ConcatenateReports(): Integer; CDECL;
begin
    if DSSPrime.ConcatenateReports then
        Result := 1
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ConcatenateReports(Value: Integer); CDECL;
begin
    DSSPrime.ConcatenateReports := (Value = 1);
end;
//------------------------------------------------------------------------------
{$ELSE}
function NotAvailable(DSS: TDSSContext): Integer;
begin
    DoSimpleMsg(DSS, 'Parallel machine functions were not compiled', 7982);
    Result := -1;
end;

function Parallel_Get_NumCPUs(): Integer; CDECL;
begin
    Result := NotAvailable(DSSPrime);
end;

function Parallel_Get_NumCores(): Integer; CDECL;
begin
    Result := NotAvailable(DSSPrime);
end;

function Parallel_Get_ActiveActor(): Integer; CDECL;
begin
    Result := NotAvailable(DSSPrime);
end;

procedure Parallel_Set_ActiveActor(Value: Integer); CDECL;
begin
    NotAvailable(DSSPrime);
end;

procedure Parallel_CreateActor(); CDECL;
begin
    NotAvailable(DSSPrime);
end;

function Parallel_Get_ActorCPU(): Integer; CDECL;
begin
    Result := NotAvailable(DSSPrime);
end;

procedure Parallel_Set_ActorCPU(Value: Integer); CDECL;
begin
    NotAvailable(DSSPrime);
end;

function Parallel_Get_NumOfActors(): Integer; CDECL;
begin
    Result := NotAvailable(DSSPrime);
end;

procedure Parallel_Wait(); CDECL;
begin
    NotAvailable(DSSPrime);
end;

procedure Parallel_Get_ActorProgress(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    NotAvailable(DSSPrime);
end;

procedure Parallel_Get_ActorProgress_GR(); CDECL;
begin
    NotAvailable(DSSPrime);
end;

procedure Parallel_Get_ActorStatus(var ResultPtr: PInteger; ResultCount: PAPISize); CDECL;
begin
    NotAvailable(DSSPrime);
end;

procedure Parallel_Get_ActorStatus_GR(); CDECL;
begin
    NotAvailable(DSSPrime);
end;

function Parallel_Get_ActiveParallel(): Integer; CDECL;
begin
    Result := NotAvailable(DSSPrime);
end;

procedure Parallel_Set_ActiveParallel(Value: Integer); CDECL;
begin
    NotAvailable(DSSPrime);
end;

function Parallel_Get_ConcatenateReports(): Integer; CDECL;
begin
    Result := NotAvailable(DSSPrime);
end;

procedure Parallel_Set_ConcatenateReports(Value: Integer); CDECL;
begin
    NotAvailable(DSSPrime);
end;
{$ENDIF}
end.
