unit CAPI_CNData;

interface

uses
    CAPI_Utils,
    CNData,
    CableData;

type
    CableDataProps = (EpsR = 1, InsLayer, DiaIns, DiaCable);
    CNDataProps = (k = 1, DiaStrand, GmrStrand, Rstrand);

procedure CableDataSetDefaults(prop: CableDataProps; conductor: TCableDataObj);

// Common to all classes
function CNData_Get_Count(): Integer; CDECL;
function CNData_Get_First(): Integer; CDECL;
function CNData_Get_Next(): Integer; CDECL;
function CNData_Get_Name(): PAnsiChar; CDECL;
procedure CNData_Set_Name(const Value: PAnsiChar); CDECL;
procedure CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
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
    CAPI_WireData;

//------------------------------------------------------------------------------  
function _activeObj(out obj: TCNDataObj): boolean; inline;
begin
    obj := NIL;
    Result := False;
    if InvalidCircuit then
        Exit;
        
    obj := CNDataClass.GetActiveObj();
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active CNData object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------  
procedure CableDataSetDefaults(prop: CableDataProps; conductor: TCableDataObj);
begin
  {Set defaults}
    with conductor do
    begin
      {Check for critical errors}
        case prop of
            CableDataProps.EpsR:
                if (FEpsR < 1.0) then
                    DoSimpleMsg('Error: Insulation permittivity must be greater than one for CableData ' + Name, 999);
            CableDataProps.InsLayer:
                if (FInsLayer <= 0.0) then
                    DoSimpleMsg('Error: Insulation layer thickness must be positive for CableData ' + Name, 999);
            CableDataProps.DiaIns:
                if (FDiaIns <= 0.0) then
                    DoSimpleMsg('Error: Diameter over insulation layer must be positive for CableData ' + Name, 999);
            CableDataProps.DiaCable:
                if (FDiaCable <= 0.0) then
                    DoSimpleMsg('Error: Diameter over cable must be positive for CableData ' + Name, 999);
        end;
    end;
end;
//------------------------------------------------------------------------------  
procedure CNDataSetDefaults(prop: CNDataProps; conductor: TCNDataObj);
begin
  {Set defaults}
    with conductor do
    begin
    {Set defaults}
        case prop of
            CNDataProps.DiaStrand:
                if FGmrStrand <= 0.0 then
                    FGmrStrand := 0.7788 * 0.5 * FDiaStrand;
        end;

    {Check for critical errors}
        case prop of
            CNDataProps.k:
                if (FkStrand < 2) then
                    DoSimpleMsg('Error: Must have at least 2 concentric neutral strands for CNData ' + Name, 999);
            CNDataProps.DiaStrand:
                if (FDiaStrand <= 0.0) then
                    DoSimpleMsg('Error: Neutral strand diameter must be positive for CNData ' + Name, 999);
            CNDataProps.GmrStrand:
                if (FGmrStrand <= 0.0) then
                    DoSimpleMsg('Error: Neutral strand GMR must be positive for CNData ' + Name, 999);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := CNDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
function CNData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := CNDataClass.First;
end;
//------------------------------------------------------------------------------
function CNData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := CNDataClass.Next;
end;
//------------------------------------------------------------------------------
function CNData_Get_Name(): PAnsiChar; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := NIL;
    if not _activeObj(pCNData) then
        Exit;

    Result := DSS_GetAsPAnsiChar(pCNData.Name);
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if InvalidCircuit then
        Exit;

    if not CNDataClass.SetActive(Value) then
        DoSimpleMsg('CNData "' + Value + '" Not Found in Active Circuit.', 51008);

     // Still same active object if not found
end;
//------------------------------------------------------------------------------
procedure CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit then
        Exit;

    Generic_Get_AllNames(ResultPtr, ResultCount, CNDataClass.ElementList, False);
end;
//------------------------------------------------------------------------------
function CNData_Get_NormAmps(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.NormAmps;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_NormAmps(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    pCNData.NormAmps := Value;
    ConductorSetDefaults(ConductorProps.NormAmps, pCNData);
end;
//------------------------------------------------------------------------------
function CNData_Get_EmergAmps(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.EmergAmps;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_EmergAmps(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    pCNData.EmergAmps := Value;
    ConductorSetDefaults(ConductorProps.EmergAmps, pCNData);
end;
//------------------------------------------------------------------------------
function CNData_Get_Diameter(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FRadius * 2.0;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Diameter(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FRadius := Value / 2.0;
        ConductorSetDefaults(ConductorProps.diam, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Radius(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FRadius;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Radius(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FRadius := Value;
        ConductorSetDefaults(ConductorProps.Radius, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_GMRac(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FGMR60;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GMRac(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FGMR60 := Value;
        ConductorSetDefaults(ConductorProps.GMRac, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Rac(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FR60;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Rac(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FR60 := Value;
        ConductorSetDefaults(ConductorProps.Rac, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Rdc(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FRDC;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Rdc(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FRDC := Value;
        ConductorSetDefaults(ConductorProps.Rdc, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_GMRUnits(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FGMRUnits;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GMRUnits(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FGMRUnits := Value;
        ConductorSetDefaults(ConductorProps.GMRunits, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_RadiusUnits(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FRadiusUnits;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_RadiusUnits(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FRadiusUnits := Value;
        ConductorSetDefaults(ConductorProps.radunits, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_ResistanceUnits(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FResistanceUnits;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FResistanceUnits := Value;
        ConductorSetDefaults(ConductorProps.Runits, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_EpsR(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FEpsR;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_EpsR(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FEpsR := Value;
        CableDataSetDefaults(CableDataProps.EpsR, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_InsLayer(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FInsLayer;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_InsLayer(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FInsLayer := Value;
        CableDataSetDefaults(CableDataProps.InsLayer, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaIns(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FDiaIns;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaIns(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;
    with pCNData do
    begin
        FDiaIns := Value;
        CableDataSetDefaults(CableDataProps.DiaIns, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaCable(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;
    Result := pCNData.FDiaCable;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaCable(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;
    with pCNData do
    begin
        FDiaCable := Value;
        CableDataSetDefaults(CableDataProps.DiaCable, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_k(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;
    Result := pCNData.FkStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_k(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;
    with pCNData do
    begin
        FkStrand := Value;
        CNDataSetDefaults(CNDataProps.k, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaStrand(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FDiaStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaStrand(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FDiaStrand := Value;
        CNDataSetDefaults(CNDataProps.DiaStrand, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_GmrStrand(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;
    Result := pCNData.FGmrStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GmrStrand(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FGmrStrand := Value;
        CNDataSetDefaults(CNDataProps.GmrStrand, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_RStrand(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if not _activeObj(pCNData) then
        Exit;

    Result := pCNData.FRStrand;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_RStrand(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if not _activeObj(pCNData) then
        Exit;

    with pCNData do
    begin
        FRStrand := Value;
        CNDataSetDefaults(CNDataProps.RStrand, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    Result := CNDataClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure CNData_Set_idx(Value: Integer); CDECL;
begin
    if CNDataClass.ElementList.Get(Value) = NIL then
        DoSimpleMsg('Invalid CNData index: "' + IntToStr(Value) + '".', 656565);
end;
//------------------------------------------------------------------------------
end.
