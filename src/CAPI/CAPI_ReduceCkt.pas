unit CAPI_ReduceCkt;

interface

uses
    CAPI_Utils;

function ReduceCkt_Get_Zmag(): Double; CDECL;
procedure ReduceCkt_Set_Zmag(Value: Double); CDECL;
function ReduceCkt_Get_KeepLoad(): TAPIBoolean; CDECL;
procedure ReduceCkt_Set_KeepLoad(Value: TAPIBoolean); CDECL;
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
    Executive,
    EnergyMeter,
    ReduceAlgs,
    PDElement,
    DSSClass,
    DSSHelper;

var
    ReduceEditString: String;
    EnergyMeterName: String;
    FirstPDelement: String;  // Full name
    
//------------------------------------------------------------------------------
function CommonReduceCktChecks(): Boolean; inline;
begin
    Result := False;
    
    if InvalidCircuit(DSSPrime) then
        Exit;
    
    if DSSPrime.EnergyMeterClass.SetActive(EnergyMeterName) then
        DSSPrime.ActiveEnergyMeterObj := DSSPrime.EnergyMeterClass.ElementList.Active;
        
    if not Assigned(DSSPrime.ActiveEnergyMeterObj) then 
        Exit;
    
    if not Assigned(DSSPrime.ActiveEnergyMeterObj.BranchList) then
        DSSPrime.ActiveEnergyMeterObj.MakeMeterZoneLists();
        
    Result := True;
end;
//------------------------------------------------------------------------------
function ReduceCkt_Get_Zmag(): Double; CDECL;
begin
    Result := 0.0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ReductionZmag
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Set_Zmag(Value: Double); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ReductionZmag := Value;
end;
//------------------------------------------------------------------------------
function ReduceCkt_Get_KeepLoad(): TAPIBoolean; CDECL;
begin
    Result := FALSE;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.ReduceLateralsKeepLoad;
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Set_KeepLoad(Value: TAPIBoolean); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    DSSPrime.ActiveCircuit.ReduceLateralsKeepLoad := Value;
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
    if InvalidCircuit(DSSPrime) then
        Exit;

    DSSPrime.DSSExecutive.Command := 'Save Circuit Dir=' + CktName;
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
    DoReduceDefault(DSSPrime, DSSPrime.ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoShortLines(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoReduceShortLines(DSSPrime, DSSPrime.ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_Do1phLaterals(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoRemoveAll_1ph_Laterals(DSSPrime, DSSPrime.ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoBranchRemove(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    if DSSPrime.ActiveCircuit.SetElementActive(FirstPDelement) < 0 then 
        Exit;
    
    // element was found (0-based array)
    DoRemoveBranches(DSSPrime, 
        DSSPrime.ActiveEnergyMeterObj.BranchList, 
        DSSPrime.ActiveCircuit.ActiveCktElement as TPDElement, 
        DSSPrime.ActiveCircuit.ReduceLateralsKeepLoad, 
        ReduceEditString
    );
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoDangling(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoReduceDangling(DSSPrime, DSSPrime.ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoLoopBreak(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoBreakLoops(DSSPrime, DSSPrime.ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoParallelLines(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoMergeParallelLines(DSSPrime, DSSPrime.ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------
procedure ReduceCkt_DoSwitches(); CDECL;
begin
    if not CommonReduceCktChecks() then Exit;
    DoRemoveAll_1ph_Laterals(DSSPrime, DSSPrime.ActiveEnergyMeterObj.BranchList);
end;
//------------------------------------------------------------------------------

initialization
    ReduceEditString := ''; // Init to null string
    EnergyMeterName := '';
    FirstPDelement := '';
end.
