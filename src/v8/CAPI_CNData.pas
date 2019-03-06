unit CAPI_CNData;

{$inline on}

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
procedure CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
    CAPI_WireData;

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
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := CNDataClass[ActiveActor].ElementCount;
end;
//------------------------------------------------------------------------------
function CNData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := CNDataClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
function CNData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := CNDataClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
function CNData_Get_Name_AnsiString(): Ansistring; inline;
var
    pCNData: TCNDataObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        if pCNData <> NIL then
        begin
            Result := pCNData.Name;
        end;
    end;

end;

function CNData_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(CNData_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if not CNDataClass[ActiveActor].SetActive(Value) then
            DoSimpleMsg('CNData "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;
//------------------------------------------------------------------------------
procedure CNData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    CNDataElem: TCNDataObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if CNDataClass[ActiveActor].ElementList.ListSize > 0 then
            begin
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (CNDataClass[ActiveActor].ElementList.ListSize - 1) + 1);
                k := 0;
                CNDataElem := CNDataClass[ActiveActor].ElementList.First;
                while CNDataElem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(CNDataElem.Name);
                    Inc(k);
                    CNDataElem := CNDataClass[ActiveActor].ElementList.Next;
                end;
            end;

end;

procedure CNData_Get_AllNames_GR(); CDECL;
// Same as CNData_Get_AllNames but uses global result (GR) pointers
begin
    CNData_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
function CNData_Get_NormAmps(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.NormAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_NormAmps(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        pCNData.NormAmps := Value;
        ConductorSetDefaults(ConductorProps.NormAmps, pCNData);
    end

end;
//------------------------------------------------------------------------------
function CNData_Get_EmergAmps(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.EmergAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_EmergAmps(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        pCNData.EmergAmps := Value;
        ConductorSetDefaults(ConductorProps.EmergAmps, pCNData);
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Diameter(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FRadius * 2.0;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Diameter(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FRadius := Value / 2.0;
            ConductorSetDefaults(ConductorProps.diam, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Radius(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FRadius;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Radius(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FRadius := Value;
            ConductorSetDefaults(ConductorProps.Radius, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_GMRac(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FGMR60;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GMRac(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FGMR60 := Value;
            ConductorSetDefaults(ConductorProps.GMRac, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Rac(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FR60;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Rac(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FR60 := Value;
            ConductorSetDefaults(ConductorProps.Rac, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_Rdc(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FRDC;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_Rdc(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FRDC := Value;
            ConductorSetDefaults(ConductorProps.Rdc, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_GMRUnits(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FGMRUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GMRUnits(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FGMRUnits := Value;
            ConductorSetDefaults(ConductorProps.GMRunits, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_RadiusUnits(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FRadiusUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_RadiusUnits(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FRadiusUnits := Value;
            ConductorSetDefaults(ConductorProps.radunits, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_ResistanceUnits(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FResistanceUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FResistanceUnits := Value;
            ConductorSetDefaults(ConductorProps.Runits, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_EpsR(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FEpsR;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_EpsR(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FEpsR := Value;
            CableDataSetDefaults(CableDataProps.EpsR, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_InsLayer(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FInsLayer;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_InsLayer(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FInsLayer := Value;
            CableDataSetDefaults(CableDataProps.InsLayer, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaIns(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FDiaIns;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaIns(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FDiaIns := Value;
            CableDataSetDefaults(CableDataProps.DiaIns, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaCable(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FDiaCable;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaCable(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FDiaCable := Value;
            CableDataSetDefaults(CableDataProps.DiaCable, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_k(): Integer; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FkStrand;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_k(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FkStrand := Value;
            CNDataSetDefaults(CNDataProps.k, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_DiaStrand(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FDiaStrand;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_DiaStrand(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FDiaStrand := Value;
            CNDataSetDefaults(CNDataProps.DiaStrand, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_GmrStrand(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FGmrStrand;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_GmrStrand(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FGmrStrand := Value;
            CNDataSetDefaults(CNDataProps.GmrStrand, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_RStrand(): Double; CDECL;
var
    pCNData: TCNDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        Result := pCNData.FRStrand;
    end;
end;
//------------------------------------------------------------------------------
procedure CNData_Set_RStrand(Value: Double); CDECL;
var
    pCNData: TCNDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pCNData := CNDataClass[ActiveActor].GetActiveObj;
        with pCNData do
        begin
            FRStrand := Value;
            CNDataSetDefaults(CNDataProps.RStrand, pCNData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function CNData_Get_idx(): Integer; CDECL;
begin
    Result := CNDataClass[ActiveActor].ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure CNData_Set_idx(Value: Integer); CDECL;
var
    pCNData: TCNDataObj;
begin
    pCNData := CNDataClass[ActiveActor].ElementList.Get(Value);
end;
//------------------------------------------------------------------------------
end.
