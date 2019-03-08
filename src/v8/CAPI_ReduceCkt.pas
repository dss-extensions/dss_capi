unit CAPI_ReduceCkt;

{$inline on}

interface

uses
    CAPI_Utils;

function ReduceCkt_Get_Zmag(): Double; CDECL;
procedure ReduceCkt_Set_Zmag(Value: Double); CDECL;
function ReduceCkt_Get_KeepLoad(): Wordbool; CDECL;
procedure ReduceCkt_Set_KeepLoad(Value: Wordbool); CDECL;
function ReduceCkt_Get_EditString(): PAnsiChar; CDECL;
procedure ReduceCkt_Set_EditString(const Value: PAnsiChar); CDECL;
function ReduceCkt_Get_StartPDElement(): PAnsiChar; CDECL;
procedure ReduceCkt_Set_StartPDElement(const Value: PAnsiChar); CDECL;
function ReduceCkt_Get_EnergyMeter(): PAnsiChar; CDECL;
procedure ReduceCkt_SaveCircuit(const CktName: PAnsiChar); CDECL;
procedure ReduceCkt_Set_EnergyMeter(const Value: PAnsiChar); CDECL;
procedure ReduceCkt_DoDefault(); CDECL;
procedure ReduceCkt_DoShortLines(); CDECL;
procedure ReduceCkt_Do1phLaterals(); CDECL;
procedure ReduceCkt_DoBranchRemove(); CDECL;
procedure ReduceCkt_DoDangling(); CDECL;
procedure ReduceCkt_DoLoopBreak(); CDECL;
procedure ReduceCkt_DoParallelLines(); CDECL;
procedure ReduceCkt_DoSwitches(); CDECL;

implementation

uses
    CAPI_Constants,
    Circuit,
    DSSGlobals,
    ComServ,
    Executive,
    EnergyMeter,
    ReduceAlgs,
    PDElement;

var
    ReduceEditString: String;
    EnergyMeterName: String;
    FirstPDelement: String;  // Full name
    
//------------------------------------------------------------------------------
function CommonReduceCktChecks(): Boolean; inline;
begin
    Result := False;
    
    if ActiveCircuit[ActiveActor] = NIL then 
        Exit;
    
    if EnergyMeterClass[ActiveActor].SetActive(EnergyMeterName) then
        ActiveEnergyMeterObj := EnergyMeterClass[ActiveActor].ElementList.Active;
        
    if not Assigned(ActiveEnergyMeterObj) then 
        Exit;
    
    if not Assigned(ActiveEnergyMeterObj.BranchList) then
        ActiveEnergyMeterObj.MakeMeterZoneLists(ActiveActor);
        
    Result := True;
end;
//------------------------------------------------------------------------------
function ReduceCkt_Get_Zmag(): Double; CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].ReductionZmag
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Set_Zmag(Value: Double); CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        ActiveCircuit[ActiveActor].ReductionZmag := Value;
end;
//------------------------------------------------------------------------------
function ReduceCkt_Get_KeepLoad(): Wordbool; CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad;
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Set_KeepLoad(Value: Wordbool); CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad := Value;
end;
//------------------------------------------------------------------------------
function ReduceCkt_Get_EditString(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(ReduceEditString);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Set_EditString(const Value: PAnsiChar); CDECL;
begin
    ReduceEditString := Value;
end;
//------------------------------------------------------------------------------
function ReduceCkt_Get_StartPDElement(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(FirstPDelement);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Set_StartPDElement(const Value: PAnsiChar); CDECL;
begin
    FirstPDelement := Value;
end;
//------------------------------------------------------------------------------
function ReduceCkt_Get_EnergyMeter(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(EnergyMeterName);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_SaveCircuit(const CktName: PAnsiChar); CDECL;
begin
    DSSExecutive[ActiveActor].Command := 'Save Circuit Dir=' + CktName;
   // Master file name is returned in DSSText.Result
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Set_EnergyMeter(const Value: PAnsiChar); CDECL;
begin
    EnergyMeterName := Value;
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoDefault(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoReduceDefault(ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoShortLines(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoReduceShortLines(ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Do1phLaterals(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoRemoveAll_1ph_Laterals(ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoBranchRemove(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    if ActiveCircuit[ActiveActor].SetElementActive(FirstPDelement) < 0 then 
        Exit;
    
    // element was found (0-based array)
    DoRemoveBranches(
        ActiveEnergyMeterObj.BranchList, 
        ActiveCircuit[ActiveActor].ActiveCktElement as TPDElement, 
        ActiveCircuit[ActiveActor].ReduceLateralsKeepLoad, 
        ReduceEditString
    );
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoDangling(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoReduceDangling(ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoLoopBreak(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoBreakLoops(ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoParallelLines(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoMergeParallelLines(ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoSwitches(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoRemoveAll_1ph_Laterals(ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------

initialization
    ReduceEditString := ''; // Init to null string
    EnergyMeterName := '';
    FirstPDelement := '';
end.
