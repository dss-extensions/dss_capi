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
PROCEDURE Parallel_Get_ActorProgress(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
PROCEDURE Parallel_Get_ActorStatus(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
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
  if NumOfActors < CPU_Cores then
  begin
    inc(NumOfActors);
    GlobalResult  :=  inttostr(NumOfActors);
    ActiveActor   :=  NumOfActors;
    ActorCPU[ActiveActor] :=  ActiveActor -1;
    DSSExecutive := TExecutive.Create;  // Make a DSS object
    Parser[ActiveActor]   :=  ParserDel.TParser.Create;
    DSSExecutive.CreateDefaultDSSItems;
  end
  else DoSimpleMsg('There are no more CPUs available', 7001);
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
  for i := 1 to NumOfActors do
  ActorHandle[i].WaitFor;
end;
//------------------------------------------------------------------------------
PROCEDURE Parallel_Get_ActorProgress(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
VAR
  Result: PIntegerArray; 
  idx : Integer;
begin
    Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (NumOfActors) - (1) + 1);
    for idx := 1 to NumOfActors do
    Begin
      Result[idx] :=  ActorPctProgress[idx];
    End;
end;
//------------------------------------------------------------------------------
PROCEDURE Parallel_Get_ActorStatus(var ResultPtr: PInteger; var ResultCount: Integer);cdecl;
VAR
  Result: PIntegerArray; 
  idx : Integer;
begin
    Result := DSS_CreateArray_PInteger(ResultPtr, ResultCount, (NumOfActors) - (1) + 1);
    for idx := 1 to NumOfActors do
      Result[idx] :=  ActorStatus[idx];  
end;
//------------------------------------------------------------------------------
function Parallel_Get_ActiveParallel():Integer;cdecl;
begin
  if Parallel_enabled then Result :=  1 else Result   :=  0;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ActiveParallel(Value: Integer);cdecl;
begin
  if Value = 1 then Parallel_enabled :=  True else Parallel_enabled :=  False;
end;
//------------------------------------------------------------------------------
function Parallel_Get_ConcatenateReports():Integer;cdecl;
begin
  if ConcatenateReports then Result := 1 else Result :=  0;
end;
//------------------------------------------------------------------------------
procedure Parallel_Set_ConcatenateReports(Value: Integer);cdecl;
begin
  if Value = 1 then ConcatenateReports  :=  True else ConcatenateReports  :=  False;
end;
//------------------------------------------------------------------------------
END.
