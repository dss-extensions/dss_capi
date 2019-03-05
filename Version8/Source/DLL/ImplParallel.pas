unit ImplParallel;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2017, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSEngine_TLB,
    StdVcl;

type
    TParallel = class(TAutoObject, IParallel)
    PROTECTED
        function Get_NumCPUs: Integer; SAFECALL;
        function Get_NumCores: Integer; SAFECALL;
        function Get_ActiveActor: Integer; SAFECALL;
        procedure Set_ActiveActor(Value: Integer); SAFECALL;
        procedure CreateActor; SAFECALL;
        function Get_ActorCPU: Integer; SAFECALL;
        procedure Set_ActorCPU(Value: Integer); SAFECALL;
        function Get_NumOfActors: Integer; SAFECALL;
        procedure Wait; SAFECALL;
        function Get_ActorProgress: Olevariant; SAFECALL;
        function Get_ActorStatus: Olevariant; SAFECALL;
        function Get_ActiveParallel: Integer; SAFECALL;
        procedure Set_ActiveParallel(Value: Integer); SAFECALL;
        function Get_ConcatenateReports: Integer; SAFECALL;
        procedure Set_ConcatenateReports(Value: Integer); SAFECALL;
    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    Dialogs,
    SysUtils,
    solution,
    Variants,
    CktElement,
    ParserDel,
    KLUSolve,
    System.Classes;

function TParallel.Get_NumCPUs: Integer;
begin
    Result := CPU_Cores;
end;

function TParallel.Get_NumCores: Integer;
begin
    Result := round(CPU_Cores / 2);
end;

function TParallel.Get_ActiveActor: Integer;
begin
    Result := ActiveActor;
end;

procedure TParallel.Set_ActiveActor(Value: Integer);
begin
    if Value <= NumOfActors then
        ActiveActor := Value
    else
        DoSimpleMsg('The actor does not exists', 7002);
end;

procedure TParallel.CreateActor;
begin
    New_Actor_Slot();
end;

function TParallel.Get_ActorCPU: Integer;
begin
    Result := ActorCPU[ActiveActor];
end;

procedure TParallel.Set_ActorCPU(Value: Integer);
begin
    if Value < CPU_Cores then
        ActorCPU[ActiveActor] := Value
    else
        DoSimpleMsg('The CPU does not exists', 7004);
end;

function TParallel.Get_NumOfActors: Integer;
begin
    Result := NumOfActors;
end;

procedure TParallel.Wait;
var
    i: Integer;
begin
    if Parallel_enabled then
        Wait4Actors(0);
end;

function TParallel.Get_ActorProgress: Olevariant;
var
    idx: Integer;
begin
    Result := VarArrayCreate([1, NumOfActors], varInteger);
    for idx := 1 to NumOfActors do
    begin
        Result[idx] := ActorPctProgress[idx];
    end;
end;

function TParallel.Get_ActorStatus: Olevariant;
var
    idx: Integer;
begin
    Result := VarArrayCreate([1, NumOfActors], varInteger);
    for idx := 1 to NumOfActors do
    begin
        REsult[idx] := ActorStatus[idx];
    end;
end;

function TParallel.Get_ActiveParallel: Integer;
begin
    if Parallel_enabled then
        Result := 1
    else
        Result := 0;
end;

procedure TParallel.Set_ActiveParallel(Value: Integer);
begin
    if Value = 1 then
        Parallel_enabled := TRUE
    else
        Parallel_enabled := FALSE;
end;

function TParallel.Get_ConcatenateReports: Integer;
begin
    if ConcatenateReports then
        Result := 1
    else
        Result := 0;
end;

procedure TParallel.Set_ConcatenateReports(Value: Integer);
begin
    if Value = 1 then
        ConcatenateReports := TRUE
    else
        ConcatenateReports := FALSE;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TParallel, Class_Parallel,
        ciInternal, tmApartment);
    IsMultiThread := TRUE;
end.
