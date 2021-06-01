unit CAPI_WireData;

interface

uses
    CAPI_Utils,
    CAPI_Types,
    WireData,
    ConductorData;

type
    ConductorProps = (Rdc = 1, Rac, Runits, GMRac, GMRunits, radius, radunits, normamps, emergamps, diam, seasons, ratings, capradius);


function WireData_Get_Count(): Integer; CDECL;
function WireData_Get_First(): Integer; CDECL;
function WireData_Get_Next(): Integer; CDECL;
function WireData_Get_Name(): PAnsiChar; CDECL;
procedure WireData_Set_Name(const Value: PAnsiChar); CDECL;
procedure WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure WireData_Get_AllNames_GR(); CDECL;
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


procedure ConductorSetDefaults(prop: ConductorProps; conductor: TConductorDataObj);

function WireData_Get_idx(): Integer; CDECL;
procedure WireData_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits,
    DSSClass,
    DSSHelper;

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
            DoSimpleMsg(DSS, 'No active WireData object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure ConductorSetDefaults(prop: ConductorProps; conductor: TConductorDataObj);
begin
  {Set defaults}
    with conductor do
    begin
        case prop of
            ConductorProps.Rdc:
                if FR60 < 0.0 then
                    FR60 := 1.02 * FRDC;
            ConductorProps.Rac:
                if FRDC < 0.0 then
                    FRDC := FR60 / 1.02;
            ConductorProps.GMRac:
                if Fradius < 0.0 then
                    Fradius := FGMR60 / 0.7788;
            ConductorProps.GMRunits:
                if FradiusUnits = 0 then
                    FradiusUnits := FGMRunits;
            ConductorProps.radius:
                if FGMR60 < 0.0 then
                    FGMR60 := 0.7788 * FRadius;
            ConductorProps.radunits:
                if FGMRUnits = 0 then
                    FGMRunits := FradiusUnits;
            ConductorProps.normamps:
                if EmergAmps < 0.0 then
                    EmergAmps := 1.5 * NormAmps;
            ConductorProps.emergamps:
                if NormAmps < 0.0 then
                    NormAmps := EmergAmps / 1.5;
            ConductorProps.diam:
                if FGMR60 < 0.0 then
                    FGMR60 := 0.7788 * FRadius;
            ConductorProps.capradius:
                if Fcapradius60 < 0.0 then
                    Fcapradius60 := Fradius;
        end;
    {Check for critical errors}
        case prop of
            ConductorProps.GMRac:
                if (Fradius = 0.0) then
                    DoSimpleMsg('Error: Radius is specified as zero for ConductorData.' + Name, 999);
            ConductorProps.radius:
                if (FGMR60 = 0.0) then
                    DoSimpleMsg('Error: GMR is specified as zero for ConductorData.' + Name, 999);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
function WireData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.First;
end;
//------------------------------------------------------------------------------
function WireData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WireDataClass.Next;
end;
//------------------------------------------------------------------------------
function WireData_Get_Name(): PAnsiChar; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := NIL;  // signify no name
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, pWireData.Name);
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if InvalidCircuit(DSSPrime) then
        Exit;
        
    if not DSSPrime.WireDataClass.SetActive(Value) then
        DoSimpleMsg(DSSPrime, 'WireData "' + Value + '" Not Found in Active Circuit.', 51008);

     // Still same active object if not found
end;
//------------------------------------------------------------------------------
procedure WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.WireDataClass.ElementList, False);
end;

procedure WireData_Get_AllNames_GR(); CDECL;
// Same as WireData_Get_AllNames but uses global result (GR) pointers
begin
    WireData_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;
//------------------------------------------------------------------------------
function WireData_Get_NormAmps(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.NormAmps;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_NormAmps(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.NormAmps := Value;
    ConductorSetDefaults(ConductorProps.NormAmps, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_EmergAmps(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_EmergAmps(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.EmergAmps := Value;
    ConductorSetDefaults(ConductorProps.EmergAmps, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_Diameter(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FRadius * 2.0;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Diameter(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;

    pWireData.FRadius := Value / 2.0;
    ConductorSetDefaults(ConductorProps.diam, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_Radius(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FRadius;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Radius(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.FRadius := Value;
    ConductorSetDefaults(ConductorProps.Radius, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_GMRac(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FGMR60;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRac(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.FGMR60 := Value;
    ConductorSetDefaults(ConductorProps.GMRac, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_Rac(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FR60;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rac(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.FR60 := Value;
    ConductorSetDefaults(ConductorProps.Rac, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_Rdc(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FRDC;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rdc(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.FRDC := Value;
    ConductorSetDefaults(ConductorProps.Rdc, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_CapRadius(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.CapRadius;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_CapRadius(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.Fcapradius60 := Value;
    ConductorSetDefaults(ConductorProps.CapRadius, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_GMRUnits(): Integer; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FGMRUnits;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;

    pWireData.FGMRUnits := Value;
    ConductorSetDefaults(ConductorProps.GMRunits, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_RadiusUnits(): Integer; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FRadiusUnits;
end;

//------------------------------------------------------------------------------
procedure WireData_Set_RadiusUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.FRadiusUnits := Value;
    ConductorSetDefaults(ConductorProps.radunits, pWireData);
end;
//------------------------------------------------------------------------------
function WireData_Get_ResistanceUnits(): Integer; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    Result := pWireData.FResistanceUnits;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if not _activeObj(DSSPrime, pWireData) then
        Exit;
    pWireData.FResistanceUnits := Value;
    ConductorSetDefaults(ConductorProps.Runits, pWireData);
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
        DoSimpleMsg(DSSPrime, 'Invalid WireData index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
