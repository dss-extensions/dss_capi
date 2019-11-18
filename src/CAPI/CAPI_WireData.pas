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
    LineUnits,
    DSSClass,
    DSSHelper;

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
                    DoSimpleMsg(DSSPrime, 'Error: Radius is specified as zero for ConductorData.' + Name, 999);
            ConductorProps.radius:
                if (FGMR60 = 0.0) then
                    DoSimpleMsg(DSSPrime, 'Error: GMR is specified as zero for ConductorData.' + Name, 999);
        end;
    end;
end;
//------------------------------------------------------------------------------
function WireData_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.WireDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
function WireData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.WireDataClass.First;
end;
//------------------------------------------------------------------------------
function WireData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
        Result := DSSPrime.WireDataClass.Next;
end;
//------------------------------------------------------------------------------
function WireData_Get_Name_AnsiString(): Ansistring; inline;
var
    pWireData: TWireDataObj;

begin
    Result := '';  // signify no name
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        if not DSSPrime.WireDataClass.SetActive(Value) then
            DoSimpleMsg(DSSPrime, 'WireData "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;
//------------------------------------------------------------------------------
procedure WireData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if DSSPrime.ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.WireDataClass.ElementList, False);
end;
//------------------------------------------------------------------------------
function WireData_Get_NormAmps(): Double; CDECL;
var
    pWireData: TWireDataObj;
begin
    Result := 0;
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.NormAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_NormAmps(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.EmergAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_EmergAmps(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FRadius * 2.0;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Diameter(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FRadius;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Radius(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FGMR60;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRac(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FR60;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rac(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FRDC;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_Rdc(Value: Double); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FGMRUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_GMRUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FRadiusUnits;
    end;
end;

//------------------------------------------------------------------------------
procedure WireData_Set_RadiusUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
        Result := pWireData.FResistanceUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure WireData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    pWireData: TWireDataObj;
begin
    if DSSPrime.ActiveCircuit <> NIL then
    begin
        pWireData := DSSPrime.WireDataClass.GetActiveObj;
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
