unit CAPI_CNData;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    CNData,
    CableData;

// Common to all classes
function CNData_Get_Count(): Integer; CDECL;
function CNData_Get_First(): Integer; CDECL;
function CNData_Get_Next(): Integer; CDECL;
function CNData_Get_Name(): PAnsiChar; CDECL;
procedure CNData_Set_Name(const Value: PAnsiChar); CDECL;
procedure CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure CNData_Get_AllNames_GR(); CDECL;
function CNData_Get_idx(): Integer; CDECL;
procedure CNData_Set_idx(Value: Integer); CDECL;


// From ConductorData
function CNData_Get_Rdc(): Double; CDECL;
procedure CNData_Set_Rdc(Value: Double); CDECL;
function CNData_Get_Rac(): Double; CDECL;
procedure CNData_Set_Rac(Value: Double); CDECL;
function CNData_Get_GMRac(): Double; CDECL;
procedure CNData_Set_GMRac(Value: Double); CDECL;
function CNData_Get_GMRUnits(): Integer; CDECL;
procedure CNData_Set_GMRUnits(Value: Integer); CDECL;
function CNData_Get_Radius(): Double; CDECL;
procedure CNData_Set_Radius(Value: Double); CDECL;
function CNData_Get_RadiusUnits(): Integer; CDECL;
procedure CNData_Set_RadiusUnits(Value: Integer); CDECL;
function CNData_Get_ResistanceUnits(): Integer; CDECL;
procedure CNData_Set_ResistanceUnits(Value: Integer); CDECL;
function CNData_Get_Diameter(): Double; CDECL;
procedure CNData_Set_Diameter(Value: Double); CDECL;
function CNData_Get_NormAmps(): Double; CDECL;
procedure CNData_Set_NormAmps(Value: Double); CDECL;
function CNData_Get_EmergAmps(): Double; CDECL;
procedure CNData_Set_EmergAmps(Value: Double); CDECL;

// From CableData
function CNData_Get_EpsR(): Double; CDECL;
procedure CNData_Set_EpsR(Value: Double); CDECL;
function CNData_Get_InsLayer(): Double; CDECL;
procedure CNData_Set_InsLayer(Value: Double); CDECL;
function CNData_Get_DiaIns(): Double; CDECL;
procedure CNData_Set_DiaIns(Value: Double); CDECL;
function CNData_Get_DiaCable(): Double; CDECL;
procedure CNData_Set_DiaCable(Value: Double); CDECL;

// From CNData
function CNData_Get_k(): Integer; CDECL;
procedure CNData_Set_k(Value: Integer); CDECL;
function CNData_Get_DiaStrand(): Double; CDECL;
procedure CNData_Set_DiaStrand(Value: Double); CDECL;
function CNData_Get_GmrStrand(): Double; CDECL;
procedure CNData_Set_GmrStrand(Value: Double); CDECL;
function CNData_Get_RStrand(): Double; CDECL;
procedure CNData_Set_RStrand(Value: Double); CDECL;

implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    ConductorData,
    CAPI_WireData,
    DSSClass,
    DSSHelper;

const
    ConductorPropOffset = ord(High(TCableDataProp)) + ord(High(TCNDataProp));
    CableDataPropOffset = ord(High(TCNDataProp));

//------------------------------------------------------------------------------  
function _activeObj(DSS: TDSSContext; out obj: TCNDataObj): boolean; inline;
begin
    obj := NIL;
    Result := False;
    if InvalidCircuit(DSS) then
        Exit;
        
    obj := DSS.CNDataClass.GetActiveObj();
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['CNData'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function CNData_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.CNDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
function CNData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.CNDataClass.First;
end;
//------------------------------------------------------------------------------
function CNData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.CNDataClass.Next;
end;
//------------------------------------------------------------------------------
function CNData_Get_Name(): PAnsiChar; CDECL;
var
    elem: TCNDataObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if not DSSPrime.CNDataClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'CNData "%s" not found in Active Circuit.', [Value], 51008);

     // Still same active object if not found
end;
//------------------------------------------------------------------------------
procedure CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;

    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.CNDataClass.ElementList, False);
end;

procedure CNData_Get_AllNames_GR(); CDECL;
// Same as CNData_Get_AllNames but uses global result (GR) pointers
begin
    CNData_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;
//------------------------------------------------------------------------------
function CNData_Get_NormAmps(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.NormAmps;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_NormAmps(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.NormAmps := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.NormAmps))
end;
//------------------------------------------------------------------------------
function CNData_Get_EmergAmps(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_EmergAmps(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.EmergAmps := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.EmergAmps))
end;
//------------------------------------------------------------------------------
function CNData_Get_Diameter(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FRadius * 2.0;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Diameter(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FRadius := Value / 2.0;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.diam))
end;
//------------------------------------------------------------------------------
function CNData_Get_Radius(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FRadius;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Radius(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FRadius := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Radius))
end;
//------------------------------------------------------------------------------
function CNData_Get_GMRac(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FGMR60;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GMRac(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FGMR60 := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRac))
end;
//------------------------------------------------------------------------------
function CNData_Get_Rac(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FR60;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Rac(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FR60 := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rac))
end;
//------------------------------------------------------------------------------
function CNData_Get_Rdc(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FRDC;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Rdc(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FRDC := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Rdc))
end;
//------------------------------------------------------------------------------
function CNData_Get_GMRUnits(): Integer; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FGMRUnits;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GMRUnits(Value: Integer); CDECL;
var
    elem: TCNDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prevVal := elem.FGMRUnits;
    elem.FGMRUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.GMRunits), prevVal)
end;
//------------------------------------------------------------------------------
function CNData_Get_RadiusUnits(): Integer; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FRadiusUnits;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_RadiusUnits(Value: Integer); CDECL;
var
    elem: TCNDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prevVal := elem.FRadiusUnits;
    elem.FRadiusUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.radunits), prevVal)
end;
//------------------------------------------------------------------------------
function CNData_Get_ResistanceUnits(): Integer; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FResistanceUnits;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    elem: TCNDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prevVal := elem.FResistanceUnits;
    elem.FResistanceUnits := Value;
    elem.PropertySideEffects(ConductorPropOffset + ord(TConductorDataProp.Runits), prevVal)
end;
//------------------------------------------------------------------------------
function CNData_Get_EpsR(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FEpsR;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_EpsR(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FEpsR := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.EpsR))
end;
//------------------------------------------------------------------------------
function CNData_Get_InsLayer(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FInsLayer;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_InsLayer(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FInsLayer := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.InsLayer))
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaIns(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FDiaIns;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaIns(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FDiaIns := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.DiaIns))
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaCable(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FDiaCable;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaCable(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FDiaCable := Value;
    elem.PropertySideEffects(CableDataPropOffset + ord(TCableDataProp.DiaCable))
end;
//------------------------------------------------------------------------------
function CNData_Get_k(): Integer; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FkStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_k(Value: Integer); CDECL;
var
    elem: TCNDataObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    prevVal := elem.FkStrand;
    elem.FkStrand := Value;
    elem.PropertySideEffects(ord(TCNDataProp.k), prevVal)
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaStrand(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FDiaStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaStrand(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FDiaStrand := Value;
    elem.PropertySideEffects(ord(TCNDataProp.DiaStrand))
end;
//------------------------------------------------------------------------------
function CNData_Get_GmrStrand(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.FGmrStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GmrStrand(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FGmrStrand := Value;
    elem.PropertySideEffects(ord(TCNDataProp.GmrStrand))
end;
//------------------------------------------------------------------------------
function CNData_Get_RStrand(): Double; CDECL;
var
    elem: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.FRStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_RStrand(Value: Double); CDECL;
var
    elem: TCNDataObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.FRStrand := Value;
    elem.PropertySideEffects(ord(TCNDataProp.RStrand))
end;
//------------------------------------------------------------------------------
function CNData_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;

    Result := DSSPrime.CNDataClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure CNData_Set_idx(Value: Integer); CDECL;
begin
    if DSSPrime.CNDataClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['CNData', Value], 656565);
end;
//------------------------------------------------------------------------------
end.
