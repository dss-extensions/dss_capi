unit CAPI_NoParallel;

interface

uses
    CAPI_Utils,
    CAPI_Types;

{$IFNDEF DSS_CAPI_PM}
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

{$IFNDEF DSS_CAPI_PM}
function NotAvailable(DSS: TDSSContext): Integer;
begin
    DoSimpleMsg(DSS, _('Parallel machine functions were not compiled'), 7982);
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
