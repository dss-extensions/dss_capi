unit CAPI_TSData;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    TSData,
    CableData;

// Common to all classes
function TSData_Get_Count(): Integer; CDECL;
function TSData_Get_First(): Integer; CDECL;
function TSData_Get_Next(): Integer; CDECL;
function TSData_Get_Name(): PAnsiChar; CDECL;
procedure TSData_Set_Name(const Value: PAnsiChar); CDECL;
procedure TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure TSData_Get_AllNames_GR(); CDECL;
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
    CAPI_CNData,
    DSSClass,
    DSSHelper;

const
    ConductorPropOffset = ord(High(TCableDataProp)) + ord(High(TTSDataProp));
    CableDataPropOffset = ord(High(TTSDataProp));

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TTSDataObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.TSDataClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['TSData'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function TSData_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.TSDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
function TSData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.TSDataClass.First;
end;
//------------------------------------------------------------------------------
function TSData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.TSDataClass.Next;
end;
//------------------------------------------------------------------------------
function TSData_Get_Name(): PAnsiChar; CDECL;
var
    elem: TTSDataObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if not DSSPrime.TSDataClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'TSData "%s" not found in Active Circuit.', [Value], 51008);

    // Still same active object if not found
end;
//------------------------------------------------------------------------------
procedure TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.TSDataClass.ElementList, False);
end;

procedure TSData_Get_AllNames_GR(); CDECL;
// Same as TSData_Get_AllNames but uses global result (GR) pointers
begin
    TSData_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;
//------------------------------------------------------------------------------
function TSData_Get_NormAmps(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NormAmps;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_NormAmps(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.NormAmps := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.NormAmps))
end;
//------------------------------------------------------------------------------
function TSData_Get_EmergAmps(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem := DSSPrime.TSDataClass.GetActiveObj;
    Result := elem.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_EmergAmps(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.EmergAmps := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.EmergAmps))
end;
//------------------------------------------------------------------------------
function TSData_Get_Diameter(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FRadius * 2.0;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Diameter(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FRadius := Value / 2.0;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.diam))
end;
//------------------------------------------------------------------------------
function TSData_Get_Radius(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FRadius;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Radius(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FRadius := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Radius))
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRac(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FGMR60;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRac(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FGMR60 := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRac))
end;
//------------------------------------------------------------------------------
function TSData_Get_Rac(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FR60;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rac(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FR60 := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rac))
end;
//------------------------------------------------------------------------------
function TSData_Get_Rdc(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FRDC;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rdc(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FRDC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rdc))
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRUnits(): Integer; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FGMRUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRUnits(Value: Integer); CDECL;
var
    elem: TTSDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    prevVal := elem.FGMRUnits;
    elem.FGMRUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRunits), prevVal)
end;
//------------------------------------------------------------------------------
function TSData_Get_RadiusUnits(): Integer; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FRadiusUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_RadiusUnits(Value: Integer); CDECL;
var
    elem: TTSDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    prevVal := elem.FRadiusUnits;
    elem.FRadiusUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.radunits), prevVal)
end;
//------------------------------------------------------------------------------
function TSData_Get_ResistanceUnits(): Integer; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FResistanceUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    elem: TTSDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    prevVal := elem.FResistanceUnits;
    elem.FResistanceUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Runits), prevVal)
end;
//------------------------------------------------------------------------------
function TSData_Get_EpsR(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FEpsR;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_EpsR(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FEpsR := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.EpsR))
end;
//------------------------------------------------------------------------------
function TSData_Get_InsLayer(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FInsLayer;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_InsLayer(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FInsLayer := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.InsLayer))
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaIns(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FDiaIns;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaIns(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FDiaIns := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.DiaIns))
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaCable(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FDiaCable;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaCable(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FDiaCable := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.DiaCable))
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaShield(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.DiaShield;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaShield(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.DiaShield := Value;
    elem.PropertySideEffects(ord(TTSDataProp.DiaShield))
end;
//------------------------------------------------------------------------------
function TSData_Get_TapeLayer(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TapeLayer;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_TapeLayer(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.TapeLayer := Value;
    elem.PropertySideEffects(ord(TTSDataProp.TapeLayer))
end;
//------------------------------------------------------------------------------
function TSData_Get_TapeLap(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.TapeLap;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_TapeLap(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.TapeLap := Value;
    elem.PropertySideEffects(ord(TTSDataProp.TapeLap))
end;
//------------------------------------------------------------------------------
function TSData_Get_idx(): Integer; CDECL;
begin
    Result := DSSPrime.TSDataClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure TSData_Set_idx(Value: Integer); CDECL;
begin
    if (DSSPrime.TSDataClass = NIL) or (DSSPrime.TSDataClass.ElementList.Get(Value) = NIL) then
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['TSData', Value], 656565);
end;
//------------------------------------------------------------------------------
end.
