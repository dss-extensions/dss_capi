unit CAPI_WireData;

{$inline on}

interface

uses
    CAPI_Utils,
    WireData,
    ConductorData;

type
  //TODO: create a simple script to extract this directly from the Pascal files from electricdss-src
    ConductorProps = (Rdc = 1, Rac, Runits, GMRac, GMRunits, radius, radunits, normamps, emergamps, diam);


function WireData_Get_Count(): Integer; CDECL;
function WireData_Get_First(): Integer; CDECL;
function WireData_Get_Next(): Integer; CDECL;
function WireData_Get_Name(): PAnsiChar; CDECL;
procedure WireData_Set_Name(const Value: PAnsiChar); CDECL;
procedure WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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

procedure ConductorSetDefaults(prop: ConductorProps; conductor: TConductorDataObj);

function WireData_Get_idx(): Integer; CDECL;
procedure WireData_Set_idx(Value: Integer); CDECL;


implementation

uses
    CAPI_Constants,
    sysutils,
    DSSGlobals,
    LineUnits;

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
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := WireDataClass[ActiveActor].ElementCount;
end;
//------------------------------------------------------------------------------
function WireData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := WireDataClass[ActiveActor].First;
end;
//------------------------------------------------------------------------------
function WireData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := WireDataClass[ActiveActor].Next;
end;
//------------------------------------------------------------------------------
function WireData_Get_Name_AnsiString(): Ansistring; inline;
var
    pWireData: TWireDataObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        if pWireData <> NIL then
        begin
            Result := pWireData.Name;
        end;
    end;

end;

function WireData_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(WireData_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        if not WireDataClass[ActiveActor].SetActive(Value) then
            DoSimpleMsg('WireData "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;
//------------------------------------------------------------------------------
procedure WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    WireDataElem: TWireDataObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if WireDataClass[ActiveActor].ElementList.ListSize > 0 then
            begin
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (WireDataClass[ActiveActor].ElementList.ListSize - 1) + 1);
                k := 0;
                WireDataElem := WireDataClass[ActiveActor].ElementList.First;
                while WireDataElem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(WireDataElem.Name);
                    Inc(k);
                    WireDataElem := WireDataClass[ActiveActor].ElementList.Next;
                end;
            end;

end;

procedure WireData_Get_AllNames_GR(); CDECL;
// Same as WireData_Get_AllNames but uses global result (GR) pointers
begin
    WireData_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
function WireData_Get_NormAmps(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.NormAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_NormAmps(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        pWireData.NormAmps := Value;
        ConductorSetDefaults(ConductorProps.NormAmps, pWireData);
    end

end;
//------------------------------------------------------------------------------
function WireData_Get_EmergAmps(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.EmergAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_EmergAmps(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        pWireData.EmergAmps := Value;
        ConductorSetDefaults(ConductorProps.EmergAmps, pWireData);
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_Diameter(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FRadius * 2.0;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Diameter(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FRadius := Value / 2.0;
            ConductorSetDefaults(ConductorProps.diam, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_Radius(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FRadius;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Radius(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FRadius := Value;
            ConductorSetDefaults(ConductorProps.Radius, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_GMRac(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FGMR60;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRac(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FGMR60 := Value;
            ConductorSetDefaults(ConductorProps.GMRac, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_Rac(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FR60;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rac(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FR60 := Value;
            ConductorSetDefaults(ConductorProps.Rac, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_Rdc(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FRDC;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rdc(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FRDC := Value;
            ConductorSetDefaults(ConductorProps.Rdc, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_GMRUnits(): Integer; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FGMRUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FGMRUnits := Value;
            ConductorSetDefaults(ConductorProps.GMRunits, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_RadiusUnits(): Integer; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FRadiusUnits;
    end;
end;

//------------------------------------------------------------------------------
procedure WireData_Set_RadiusUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FRadiusUnits := Value;
            ConductorSetDefaults(ConductorProps.radunits, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_ResistanceUnits(): Integer; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        Result := pWireData.FResistanceUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pWireData := WireDataClass[ActiveActor].GetActiveObj;
        with pWireData do
        begin
            FResistanceUnits := Value;
            ConductorSetDefaults(ConductorProps.Runits, pWireData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_idx(): Integer; CDECL;
begin
    Result := WireDataClass[ActiveActor].ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure WireData_Set_idx(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    pWireData := WireDataClass[ActiveActor].ElementList.Get(Value);
end;
//------------------------------------------------------------------------------
end.
