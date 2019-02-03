UNIT CAPI_Parallel;
{$inline on}

INTERFACE

USES CAPI_Utils;

function Parallel_Get_NumCPUs():Integer;cdecl;
function Parallel_Get_NumCores():Integer;cdecl;
function Parallel_Get_ActiveActor():Integer;cdecl;
procedure Parallel_Set_ActiveActor(Value: Integer);cdecl;
procedure Parallel_CreateActor();cdecl;
function Parallel_Get_ActorCPU():Integer;cdecl;
procedure Parallel_Set_ActorCPU(Value: Integer);cdecl;
function Parallel_Get_NumOfActors():Integer;cdecl;
procedure Parallel_Wait();cdecl;
PROCEDURE Parallel_Get_ActorProgress(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
PROCEDURE Parallel_Get_ActorProgress_GR();cdecl;
PROCEDURE Parallel_Get_ActorStatus(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
PROCEDURE Parallel_Get_ActorStatus_GR();cdecl;
function Parallel_Get_ActiveParallel():Integer;cdecl;
procedure Parallel_Set_ActiveParallel(Value: Integer);cdecl;
function Parallel_Get_ConcatenateReports():Integer;cdecl;
procedure Parallel_Set_ConcatenateReports(Value: Integer);cdecl;

IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, SysUtils, solution, CktElement, ParserDel, KLUSolve, Classes;

function Parallel_Get_NumCPUs():Integer;cdecl;
Begin
    Result := CPU_Cores;
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumCores():Integer;cdecl;
Begin
    Result := round(CPU_Cores/2);
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActiveActor():Integer;cdecl;
begin
   Result := ActiveActor;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActiveActor(Value: Integer);cdecl;
begin
  if Value <= NumOfActors then ActiveActor  :=  Value
  else  DoSimpleMsg('The actor does not exists',7002);
end;
//------------------------------------------------------------------------------
procedure Parallel_CreateActor();cdecl;
begin
  New_Actor_Slot();
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActorCPU():Integer;cdecl;
begin
  Result  :=  ActorCPU[ActiveActor];
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActorCPU(Value: Integer);cdecl;
begin
  if Value < CPU_Cores  then  ActorCPU[ActiveActor] :=  Value
  else DoSimpleMsg('The CPU does not exists',7004);
end;
//------------------------------------------------------------------------------
function Parallel_Get_NumOfActors():Integer;cdecl;
begin
  Result :=  NumOfActors;
end;
//------------------------------------------------------------------------------
procedure Parallel_Wait();cdecl;
var
  i : Integer;
begin
  if Parallel_enabled then Wait4Actors(0);
end;
//------------------------------------------------------------------------------
PROCEDURE Parallel_Get_ActorProgress(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
VAR
  Result: PIntegerArray; 
  idx : Integer;
begin
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, NumOfActors);
    for idx := 1 to NumOfActors do
    Begin
      Result[idx - 1] :=  ActorPctProgress[idx];
    End;
end;
PROCEDURE Parallel_Get_ActorProgress_GR();cdecl;
// Same as Parallel_Get_ActorProgress but uses global result (GR) pointers
begin
   Parallel_Get_ActorProgress(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
PROCEDURE Parallel_Get_ActorStatus(var ResultPtr: PInteger; ResultCount: PInteger);cdecl;
VAR
  Result: PIntegerArray; 
  idx : Integer;
begin
    Result := DSS_RecreateArray_PInteger(ResultPtr, ResultCount, NumOfActors);
    for idx := 1 to NumOfActors do
    Begin
      if ActorHandle[idx].Is_Busy then Result[idx - 1] :=  0
      else Result[idx - 1] :=  1;
    End;
end;
PROCEDURE Parallel_Get_ActorStatus_GR();cdecl;
// Same as Parallel_Get_ActorStatus but uses global result (GR) pointers
begin
   Parallel_Get_ActorStatus(GR_DataPtr_PInteger, GR_CountPtr_PInteger)
end;

//------------------------------------------------------------------------------
function Parallel_Get_ActiveParallel():Integer;cdecl;
begin
  if Parallel_enabled then Result :=  1 else Result   :=  0;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActiveParallel(Value: Integer);cdecl;
begin
  Parallel_enabled := (Value = 1);
end;
//------------------------------------------------------------------------------
function Parallel_Get_ConcatenateReports():Integer;cdecl;
begin
  if ConcatenateReports then Result := 1 else Result :=  0;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ConcatenateReports(Value: Integer);cdecl;
begin
  ConcatenateReports := (Value = 1);
end;
//------------------------------------------------------------------------------
END.
