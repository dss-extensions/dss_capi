unit CAPI_TSData;

{$inline on}

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
procedure TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
    CAPI_CNData;

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
    if ActiveCircuit <> NIL then
        Result := TSDataClass.ElementCount;
end;
//------------------------------------------------------------------------------
function TSData_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := TSDataClass.First;
end;
//------------------------------------------------------------------------------
function TSData_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
        Result := TSDataClass.Next;
end;
//------------------------------------------------------------------------------
function TSData_Get_Name_AnsiString(): Ansistring; inline;
var
    pTSData: TTSDataObj;

begin
    Result := '';  // signify no name
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        if pTSData <> NIL then
        begin
            Result := pTSData.Name;
        end;
    end;

end;

function TSData_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(TSData_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Name(const Value: PAnsiChar); CDECL;
// set LineCode active by name

begin
    if ActiveCircuit <> NIL then
    begin
        if not TSDataClass.SetActive(Value) then
            DoSimpleMsg('TSData "' + Value + '" Not Found in Active Circuit.', 51008);

         // Still same active object if not found
    end;

end;
//------------------------------------------------------------------------------
procedure TSData_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    TSDataElem: TTSDataObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if TSDataClass.ElementList.ListSize > 0 then
            begin
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (TSDataClass.ElementList.ListSize - 1) + 1);
                k := 0;
                TSDataElem := TSDataClass.ElementList.First;
                while TSDataElem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(TSDataElem.Name);
                    Inc(k);
                    TSDataElem := TSDataClass.ElementList.Next;
                end;
            end;

end;

procedure TSData_Get_AllNames_GR(); CDECL;
// Same as TSData_Get_AllNames but uses global result (GR) pointers
begin
    TSData_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;
//------------------------------------------------------------------------------
function TSData_Get_NormAmps(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.NormAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_NormAmps(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        pTSData.NormAmps := Value;
        ConductorSetDefaults(ConductorProps.NormAmps, pTSData);
    end

end;
//------------------------------------------------------------------------------
function TSData_Get_EmergAmps(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.EmergAmps;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_EmergAmps(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        pTSData.EmergAmps := Value;
        ConductorSetDefaults(ConductorProps.EmergAmps, pTSData);
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_Diameter(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FRadius * 2.0;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Diameter(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FRadius := Value / 2.0;
            ConductorSetDefaults(ConductorProps.diam, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_Radius(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FRadius;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Radius(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FRadius := Value;
            ConductorSetDefaults(ConductorProps.Radius, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRac(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FGMR60;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRac(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FGMR60 := Value;
            ConductorSetDefaults(ConductorProps.GMRac, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_Rac(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FR60;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rac(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FR60 := Value;
            ConductorSetDefaults(ConductorProps.Rac, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_Rdc(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FRDC;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_Rdc(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FRDC := Value;
            ConductorSetDefaults(ConductorProps.Rdc, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_GMRUnits(): Integer; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FGMRUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_GMRUnits(Value: Integer); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FGMRUnits := Value;
            ConductorSetDefaults(ConductorProps.GMRunits, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_RadiusUnits(): Integer; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FRadiusUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_RadiusUnits(Value: Integer); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FRadiusUnits := Value;
            ConductorSetDefaults(ConductorProps.radunits, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_ResistanceUnits(): Integer; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FResistanceUnits;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_ResistanceUnits(Value: Integer); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FResistanceUnits := Value;
            ConductorSetDefaults(ConductorProps.Runits, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_EpsR(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FEpsR;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_EpsR(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FEpsR := Value;
            CableDataSetDefaults(CableDataProps.EpsR, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_InsLayer(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FInsLayer;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_InsLayer(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FInsLayer := Value;
            CableDataSetDefaults(CableDataProps.InsLayer, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaIns(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FDiaIns;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaIns(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FDiaIns := Value;
            CableDataSetDefaults(CableDataProps.DiaIns, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaCable(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FDiaCable;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaCable(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FDiaCable := Value;
            CableDataSetDefaults(CableDataProps.DiaCable, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_DiaShield(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FDiaShield;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_DiaShield(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FDiaShield := Value;
            TSDataSetDefaults(TSDataProps.DiaShield, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_TapeLayer(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FTapeLayer;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_TapeLayer(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FTapeLayer := Value;
            TSDataSetDefaults(TSDataProps.TapeLayer, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_TapeLap(): Double; CDECL;
var
    pTSData: TTSDataObj;
begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        Result := pTSData.FTapeLap;
    end;
end;
//------------------------------------------------------------------------------
procedure TSData_Set_TapeLap(Value: Double); CDECL;
var
    pTSData: TTSDataObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pTSData := TSDataClass.GetActiveObj;
        with pTSData do
        begin
            FTapeLap := Value;
            TSDataSetDefaults(TSDataProps.TapeLap, pTSData);
        end;
    end;
end;
//------------------------------------------------------------------------------
function TSData_Get_idx(): Integer; CDECL;
begin
    Result := TSDataClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure TSData_Set_idx(Value: Integer); CDECL;
var
    pTSData: TTSDataObj;
begin
    pTSData := TSDataClass.ElementList.Get(Value);
end;
//------------------------------------------------------------------------------
end.
