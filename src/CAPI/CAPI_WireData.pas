unit CAPI_WireData;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    WireData,
    ConductorData;

function WireData_Get_Count(): Integer; CDECL;
function WireData_Get_First(): Integer; CDECL;
function WireData_Get_Next(): Integer; CDECL;
function WireData_Get_Name(): PAnsiChar; CDECL;
procedure WireData_Set_Name(const Value: PAnsiChar); CDECL;
procedure WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function WireData_Get_Rdc(): Double; CDECL;
procedure WireData_Set_Rdc(Value: Double); CDECL;
function WireData_Get_Rac(): Double; CDECL;
procedure WireData_Set_Rac(Value: Double); CDECL;
function WireData_Get_GMRac(): Double; CDECL;
procedure WireData_Set_GMRac(Value: Double); CDECL;
function WireData_Get_GMRUnits(): Integer; CDECL;
procedure WireData_Set_GMRUnits(Value: Integer); CDECL;
function WireData_Get_Radius(): Double; CDECL;
procedure WireData_Set_Radius(Value: Double); CDECL;
function WireData_Get_RadiusUnits(): Integer; CDECL;
procedure WireData_Set_RadiusUnits(Value: Integer); CDECL;
function WireData_Get_ResistanceUnits(): Integer; CDECL;
procedure WireData_Set_ResistanceUnits(Value: Integer); CDECL;
function WireData_Get_Diameter(): Double; CDECL;
procedure WireData_Set_Diameter(Value: Double); CDECL;
function WireData_Get_NormAmps(): Double; CDECL;
procedure WireData_Set_NormAmps(Value: Double); CDECL;
function WireData_Get_EmergAmps(): Double; CDECL;
procedure WireData_Set_EmergAmps(Value: Double); CDECL;
function WireData_Get_CapRadius(): Double; CDECL;
procedure WireData_Set_CapRadius(Value: Double); CDECL;
function WireData_Get_idx(): Integer; CDECL;
procedure WireData_Set_idx(Value: Integer); CDECL;
function WireData_Get_Pointer(): Pointer; CDECL;

implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    DSSClass,
    DSSHelper;

const
    ConductorPropOffset = 0;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TWireDataObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.WireDataClass.GetActiveObj();
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['WireData'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function WireData_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.ElementCount();
end;
//------------------------------------------------------------------------------
function WireData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.First();
end;
//------------------------------------------------------------------------------
function WireData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.Next();
end;
//------------------------------------------------------------------------------
function WireData_Get_Name(): PAnsiChar; CDECL;
var
    elem: TWireDataObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    if not DSSPrime.WireDataClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'WireData "%s" not found in Active Circuit.', [Value], 51008);

     // Still same active object if not found
end;
//------------------------------------------------------------------------------
procedure WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.WireDataClass.ElementList, False);
end;
//------------------------------------------------------------------------------
function WireData_Get_NormAmps(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NormAmps;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_NormAmps(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.NormAmps := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.NormAmps), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_EmergAmps(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_EmergAmps(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.EmergAmps := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.EmergAmps), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_Diameter(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.radius * 2.0;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Diameter(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.radius := Value / 2.0;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.diam), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_Radius(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.radius;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Radius(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.radius := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Radius), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_GMRac(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.GMRAC;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRac(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.GMRAC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRac), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_Rac(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RAC;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rac(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.RAC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rac), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_Rdc(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RDC;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rdc(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.RDC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rdc), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_CapRadius(): Double; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.capRadius;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_CapRadius(Value: Double); CDECL;
var
    elem: TWireDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.capRadius := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.capRadius), 0, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_GMRUnits(): Integer; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.GMRUnits;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRUnits(Value: Integer); CDECL;
var
    elem: TWireDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prevVal := elem.GMRUnits;
    elem.GMRUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRunits), prevVal, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_RadiusUnits(): Integer; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.radiusUnits;
end;
//----------------his show is a genius, every contestant is so wholesome and (often unintentionally) hilarious. Eddy's enthusiasm during the Greggs round was so cute! And Jimmy is on fire as usual.--------------------------------------------------------------
procedure WireData_Set_RadiusUnits(Value: Integer); CDECL;
var
    elem: TWireDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prevVal := elem.radiusUnits;
    elem.radiusUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.radunits), prevVal, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_ResistanceUnits(): Integer; CDECL;
var
    elem: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.resistanceUnits;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    elem: TWireDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prevVal := elem.resistanceUnits;
    elem.resistanceUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Runits), prevVal, [])
end;
//------------------------------------------------------------------------------
function WireData_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure WireData_Set_idx(Value: Integer); CDECL;
begin
    if DSSPrime.WireDataClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['WireData', Value], 656565);
end;
//------------------------------------------------------------------------------
function WireData_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.GetActiveObj()
end;
//------------------------------------------------------------------------------
end.
