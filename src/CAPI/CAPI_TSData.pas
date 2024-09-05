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

function TSData_Get_Pointer(): Pointer; CDECL;

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
    
    obj := DSS.TSDataClass.GetActiveObj();
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
    Result := DSSPrime.TSDataClass.ElementCount();
end;
//------------------------------------------------------------------------------
function TSData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.TSDataClass.First();
end;
//------------------------------------------------------------------------------
function TSData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.TSDataClass.Next();
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
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.TSDataClass.ElementList, False);
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
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.NormAmps), 0, []);
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
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.EmergAmps), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_Diameter(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.radius * 2.0;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Diameter(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.radius := Value / 2.0;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.diam), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_Radius(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.radius;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Radius(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.radius := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Radius), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRac(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.GMRAC;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRac(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.GMRAC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRac), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_Rac(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RAC;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rac(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.RAC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rac), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_Rdc(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RDC;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rdc(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.RDC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rdc), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRUnits(): Integer; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.GMRUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRUnits(Value: Integer); CDECL;
var
    elem: TTSDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    prevVal := elem.GMRUnits;
    elem.GMRUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRunits), prevVal, [])
end;
//------------------------------------------------------------------------------
function TSData_Get_RadiusUnits(): Integer; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.radiusUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_RadiusUnits(Value: Integer); CDECL;
var
    elem: TTSDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    prevVal := elem.radiusUnits;
    elem.radiusUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.radunits), prevVal, [])
end;
//------------------------------------------------------------------------------
function TSData_Get_ResistanceUnits(): Integer; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.resistanceUnits;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    elem: TTSDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    prevVal := elem.resistanceUnits;
    elem.resistanceUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Runits), prevVal, [])
end;
//------------------------------------------------------------------------------
function TSData_Get_EpsR(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.epsR;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_EpsR(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.epsR := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.EpsR), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_InsLayer(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.insLayer;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_InsLayer(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.insLayer := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.insLayer), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaIns(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.diaIns;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaIns(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.diaIns := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.DiaIns), 0, []);
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaCable(): Double; CDECL;
var
    elem: TTSDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.diaCable;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaCable(Value: Double); CDECL;
var
    elem: TTSDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.diaCable := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.DiaCable), 0, []);
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
    elem.PropertySideEffects(ord(TTSDataProp.DiaShield), 0, []);
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
    elem.PropertySideEffects(ord(TTSDataProp.TapeLayer), 0, []);
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
    elem.PropertySideEffects(ord(TTSDataProp.TapeLap), 0, []);
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
function TSData_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.TSDataClass.GetActiveObj()
end;
//------------------------------------------------------------------------------
end.
