unit CAPI_Parallel;

interface

uses
    CAPI_Utils,
    CAPI_Types;

{$IFDEF DSS_CAPI_PM}
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
{$ENDIF}
implementation
{$IFDEF DSS_CAPI_PM}
uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    SysUtils,
    solution,
    CktElement,
    KLUSolve,
    Classes,
    DSSClass,
    DSSHelper;


function Parallel_Get_NumCPUs(): Integer; CDECL;
begin
    Result := CPU_Cores;
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumCores(): Integer; CDECL;
begin
    Result := CPU_Cores div 2; //TODO: fix
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActiveActor(): Integer; CDECL;
begin
    Result := DSSPrime.ActiveChildIndex + 1;
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
        DoSimpleMsg(DSSPrime, _('The actor does not exists'), 7002);
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
    else DoSimpleMsg(DSSPrime, _('The CPU does not exist'), 7004);
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumOfActors(): Integer; CDECL;
begin
    Result := DSSPrime.NumOfActors;
end;
//------------------------------------------------------------------------------
procedure Parallel_Wait(); CDECL;
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
        Result[idx] := Ord(DSSPrime.Children[idx].ActorStatus);
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
    DSSPrime.Parallel_enabled := (Value = 1);
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
{$ENDIF}
end.
