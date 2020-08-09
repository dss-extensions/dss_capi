unit CAPI_TSData;

interface

uses
    CAPI_Utils,
    TSData,
    CableData;

type
    TSDataProps = (DiaShield = 1, TapeLayer, TapeLap);

// Common to all classes
function TSData_Get_Count(): Integer; CDECL;
function TSData_Get_First(): Integer; CDECL;
function TSData_Get_Next(): Integer; CDECL;
function TSData_Get_Name(): PAnsiChar; CDECL;
procedure TSData_Set_Name(const Value: PAnsiChar); CDECL;
procedure TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function TSData_Get_idx(): Integer; CDECL;
procedure TSData_Set_idx(Value: Integer); CDECL;

// From ConductorData
function TSData_Get_Rdc(): Double; CDECL;
procedure TSData_Set_Rdc(Value: Double); CDECL;
function TSData_Get_Rac(): Double; CDECL;
procedure TSData_Set_Rac(Value: Double); CDECL;
function TSData_Get_GMRac(): Double; CDECL;
procedure TSData_Set_GMRac(Value: Double); CDECL;
function TSData_Get_GMRUnits(): Integer; CDECL;
procedure TSData_Set_GMRUnits(Value: Integer); CDECL;
function TSData_Get_Radius(): Double; CDECL;
procedure TSData_Set_Radius(Value: Double); CDECL;
function TSData_Get_RadiusUnits(): Integer; CDECL;
procedure TSData_Set_RadiusUnits(Value: Integer); CDECL;
function TSData_Get_ResistanceUnits(): Integer; CDECL;
procedure TSData_Set_ResistanceUnits(Value: Integer); CDECL;
function TSData_Get_Diameter(): Double; CDECL;
procedure TSData_Set_Diameter(Value: Double); CDECL;
function TSData_Get_NormAmps(): Double; CDECL;
procedure TSData_Set_NormAmps(Value: Double); CDECL;
function TSData_Get_EmergAmps(): Double; CDECL;
procedure TSData_Set_EmergAmps(Value: Double); CDECL;

// From CableData
function TSData_Get_EpsR(): Double; CDECL;
procedure TSData_Set_EpsR(Value: Double); CDECL;
function TSData_Get_InsLayer(): Double; CDECL;
procedure TSData_Set_InsLayer(Value: Double); CDECL;
function TSData_Get_DiaIns(): Double; CDECL;
procedure TSData_Set_DiaIns(Value: Double); CDECL;
function TSData_Get_DiaCable(): Double; CDECL;
procedure TSData_Set_DiaCable(Value: Double); CDECL;

// From TSData
function TSData_Get_DiaShield(): Double; CDECL;
procedure TSData_Set_DiaShield(Value: Double); CDECL;
function TSData_Get_TapeLayer(): Double; CDECL;
procedure TSData_Set_TapeLayer(Value: Double); CDECL;
function TSData_Get_TapeLap(): Double; CDECL;
procedure TSData_Set_TapeLap(Value: Double); CDECL;

implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    ConductorData,
    CAPI_WireData,
    CAPI_CNData;

//------------------------------------------------------------------------------
function _activeObj(out obj: TTSDataObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := TSDataClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active TSData object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------  
procedure TSDataSetDefaults(prop: TSDataProps; conductor: TTSDataObj);
begin
  {Set defaults}
    with conductor do
    begin
    {Check for critical errors}
        case prop of
            TSDataProps.DiaShield:
                if (FDiaShield <= 0.0) then
                    DoSimpleMsg('Error: Diameter over shield must be positive for TapeShieldData ' + Name, 999);
            TSDataProps.TapeLayer:
                if (FTapeLayer <= 0.0) then
                    DoSimpleMsg('Error: Tape shield thickness must be positive for TapeShieldData ' + Name, 999);
            TSDataProps.TapeLap:
                if ((FTapeLap < 0.0) or (FTapeLap > 100.0)) then
                    DoSimpleMsg('Error: Tap lap must range from 0 to 100 for TapeShieldData ' + Name, 999);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := TSDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
function TSData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := TSDataClass.First;
end;
//------------------------------------------------------------------------------
function TSData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := TSDataClass.Next;
end;
//------------------------------------------------------------------------------
function TSData_Get_Name(): PAnsiChar; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := NIL;
    if not _activeObj(pTSData) then
        Exit;
    Result := DSS_GetAsPAnsiChar(pTSData.Name);
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if InvalidCircuit then
        Exit;

    if not TSDataClass.SetActive(Value) then
        DoSimpleMsg('TSData "' + Value + '" Not Found in Active Circuit.', 51008);

    // Still same active object if not found
end;
//------------------------------------------------------------------------------
procedure TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, TSDataClass.ElementList, False);
end;
//------------------------------------------------------------------------------
function TSData_Get_NormAmps(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.NormAmps;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_NormAmps(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.NormAmps := Value;
    ConductorSetDefaults(ConductorProps.NormAmps, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_EmergAmps(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    pTSData := TSDataClass.GetActiveObj;
    Result := pTSData.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_EmergAmps(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.EmergAmps := Value;
    ConductorSetDefaults(ConductorProps.EmergAmps, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_Diameter(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FRadius * 2.0;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Diameter(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FRadius := Value / 2.0;
    ConductorSetDefaults(ConductorProps.diam, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_Radius(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FRadius;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Radius(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FRadius := Value;
    ConductorSetDefaults(ConductorProps.Radius, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRac(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FGMR60;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRac(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FGMR60 := Value;
    ConductorSetDefaults(ConductorProps.GMRac, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_Rac(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FR60;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rac(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FR60 := Value;
    ConductorSetDefaults(ConductorProps.Rac, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_Rdc(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FRDC;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rdc(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FRDC := Value;
    ConductorSetDefaults(ConductorProps.Rdc, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRUnits(): Integer; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FGMRUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRUnits(Value: Integer); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FGMRUnits := Value;
    ConductorSetDefaults(ConductorProps.GMRunits, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_RadiusUnits(): Integer; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FRadiusUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_RadiusUnits(Value: Integer); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FRadiusUnits := Value;
    ConductorSetDefaults(ConductorProps.radunits, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_ResistanceUnits(): Integer; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FResistanceUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FResistanceUnits := Value;
    ConductorSetDefaults(ConductorProps.Runits, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_EpsR(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FEpsR;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_EpsR(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FEpsR := Value;
    CableDataSetDefaults(CableDataProps.EpsR, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_InsLayer(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FInsLayer;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_InsLayer(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FInsLayer := Value;
    CableDataSetDefaults(CableDataProps.InsLayer, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaIns(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FDiaIns;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaIns(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;

    pTSData.FDiaIns := Value;
    CableDataSetDefaults(CableDataProps.DiaIns, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaCable(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FDiaCable;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaCable(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FDiaCable := Value;
    CableDataSetDefaults(CableDataProps.DiaCable, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaShield(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FDiaShield;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaShield(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FDiaShield := Value;
    TSDataSetDefaults(TSDataProps.DiaShield, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_TapeLayer(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FTapeLayer;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_TapeLayer(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FTapeLayer := Value;
    TSDataSetDefaults(TSDataProps.TapeLayer, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_TapeLap(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(pTSData) then
        Exit;
    Result := pTSData.FTapeLap;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_TapeLap(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if not _activeObj(pTSData) then
        Exit;
    pTSData.FTapeLap := Value;
    TSDataSetDefaults(TSDataProps.TapeLap, pTSData);
end;
//------------------------------------------------------------------------------
function TSData_Get_idx(): Integer; CDECL;
begin
    Result := TSDataClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure TSData_Set_idx(Value: Integer); CDECL;
begin
    if (TSDataClass = NIL) or (TSDataClass.ElementList.Get(Value) = NIL) then
        DoSimpleMsg('Invalid TSData index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
