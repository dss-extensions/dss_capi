unit CAPI_Storages;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Storages_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function Storages_Get_Count(): Integer; CDECL;
function Storages_Get_First(): Integer; CDECL;
function Storages_Get_Name(): PAnsiChar; CDECL;
function Storages_Get_Next(): Integer; CDECL;
function Storages_Get_idx(): Integer; CDECL;
procedure Storages_Set_idx(Value: Integer); CDECL;
procedure Storages_Set_Name(const Value: PAnsiChar); CDECL;
procedure Storages_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Storages_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Storages_Get_RegisterValues_GR(); CDECL;
function Storages_Get_puSOC(): Double; CDECL;
procedure Storages_Set_puSOC(Value: Double); CDECL;
function Storages_Get_State(): Integer; CDECL;
procedure Storages_Set_State(Value: Integer); CDECL;

function Storages_Get_AmpLimit(): Double; CDECL;
function Storages_Get_AmpLimitGain(): Double; CDECL;
function Storages_Get_ChargeTrigger(): Double; CDECL;
function Storages_Get_ControlMode(): Integer; CDECL;
function Storages_Get_DischargeTrigger(): Double; CDECL;
function Storages_Get_EffCharge(): Double; CDECL;
function Storages_Get_EffDischarge(): Double; CDECL;
function Storages_Get_Kp(): Double; CDECL;
function Storages_Get_kV(): Double; CDECL;
function Storages_Get_kVA(): Double; CDECL;
function Storages_Get_kvar(): Double; CDECL;
function Storages_Get_kVDC(): Double; CDECL;
function Storages_Get_kW(): Double; CDECL;
function Storages_Get_kWhRated(): Double; CDECL;
function Storages_Get_kWRated(): Double; CDECL;
function Storages_Get_LimitCurrent(): TAPIBoolean; CDECL;
function Storages_Get_PF(): Double; CDECL;
function Storages_Get_PITol(): Double; CDECL;
function Storages_Get_SafeMode(): Integer; CDECL;
function Storages_Get_SafeVoltage(): Double; CDECL;
function Storages_Get_TimeChargeTrig(): Double; CDECL;
function Storages_Get_VarFollowInverter(): Integer; CDECL;
procedure Storages_Set_AmpLimit(Value: Double); CDECL;
procedure Storages_Set_AmpLimitGain(Value: Double); CDECL;
procedure Storages_Set_ChargeTrigger(Value: Double); CDECL;
procedure Storages_Set_ControlMode(Value: Integer); CDECL;
procedure Storages_Set_DischargeTrigger(Value: Double); CDECL;
procedure Storages_Set_EffCharge(Value: Double); CDECL;
procedure Storages_Set_EffDischarge(Value: Double); CDECL;
procedure Storages_Set_Kp(Value: Double); CDECL;
procedure Storages_Set_kV(Value: Double); CDECL;
procedure Storages_Set_kVA(Value: Double); CDECL;
procedure Storages_Set_kvar(Value: Double); CDECL;
procedure Storages_Set_kVDC(Value: Double); CDECL;
procedure Storages_Set_kW(Value: Double); CDECL;
procedure Storages_Set_kWhRated(Value: Double); CDECL;
procedure Storages_Set_kWRated(Value: Double); CDECL;
procedure Storages_Set_LimitCurrent(Value: TAPIBoolean); CDECL;
procedure Storages_Set_PF(Value: Double); CDECL;
procedure Storages_Set_PITol(Value: Double); CDECL;
procedure Storages_Set_SafeVoltage(Value: Double); CDECL;
procedure Storages_Set_TimeChargeTrig(Value: Double); CDECL;
procedure Storages_Set_VarFollowInverter(Value: Integer); CDECL;

function Storages_Get_Pointer(): Pointer; CDECL;

implementation

uses
    CAPI_Constants,
    Executive,
    Sysutils,
    Storage,
    DSSPointerlist,
    DSSGlobals,
    DSSClass,
    DSSObjectHelper,
    DSSHelper;


//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TStorageObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.StorageElements.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Storage'], 18989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Storages_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.StorageElements, False);
end;
//------------------------------------------------------------------------------
function Storages_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.StorageElements.Count;
end;
//------------------------------------------------------------------------------
function Storages_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.StorageElements);
end;
//------------------------------------------------------------------------------
function Storages_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.StorageElements);
end;
//------------------------------------------------------------------------------
function Storages_Get_Name(): PAnsiChar; CDECL;
var
    elem: TStorageObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name());
end;
//------------------------------------------------------------------------------
procedure Storages_Set_Name(const Value: PAnsiChar); CDECL;
// Set element active by name
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.StorageClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.SetActiveCktElement(DSSPrime.StorageClass.ElementList.Active);
        DSSPrime.ActiveCircuit.StorageElements.Get(DSSPrime.StorageClass.ActiveIndex());
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Storage "%s" not found in Active Circuit.', [Value], 77003);
    end;
end;
//------------------------------------------------------------------------------
function Storages_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.StorageElements.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Storages_Set_idx(Value: Integer); CDECL;
var
    pStorage: TStorageObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pStorage := DSSPrime.ActiveCircuit.StorageElements.Get(Value);
    if pStorage = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Storage', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.SetActiveCktElement(pStorage);
end;
//------------------------------------------------------------------------------
procedure Storages_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumStorageRegisters);
    for k := 0 to NumStorageRegisters - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(DSSPrime.StorageClass.RegisterNames[k]);
    end;
end;
//------------------------------------------------------------------------------
procedure Storages_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    elem: TStorageObj;
    k: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumStorageRegisters);
    for k := 0 to NumStorageRegisters - 1 do
    begin
        Result[k] := elem.Registers[k + 1];
    end;
end;

procedure Storages_Get_RegisterValues_GR(); CDECL;
// Same as Storages_Get_RegisterValues but uses global result (GR) pointers
begin
    Storages_Get_RegisterValues(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
function Storages_Get_puSOC(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Storagevars.kWhStored / elem.StorageVars.kWhRating;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_puSOC(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kWhstored), elem.StorageVars.kWhRating * Value, []);
        Exit;
    end;
    elem.Storagevars.kWhStored := elem.StorageVars.kWhRating * Value;
end;
//------------------------------------------------------------------------------
function Storages_Get_State(): Integer; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.StorageState();
end;
//------------------------------------------------------------------------------
procedure Storages_Set_State(Value: Integer); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (Value <> STORE_CHARGING) and 
       (Value <> STORE_IDLING) and
       (Value <> STORE_DISCHARGING) then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid Storage state: "%d".', [Value], 656568);
    end;
    elem.SetStorageState(Value);
end;
//------------------------------------------------------------------------------
function Storages_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.StorageElements.Active
end;
//------------------------------------------------------------------------------
function Storages_Get_AmpLimit(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.dynVars.ILimit;
end;
//------------------------------------------------------------------------------
function Storages_Get_AmpLimitGain(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.dynVars.VError;
end;
//------------------------------------------------------------------------------
function Storages_Get_ChargeTrigger(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.ChargeTrigger;
end;
//------------------------------------------------------------------------------
function Storages_Get_ControlMode(): Integer; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.GFM_mode then
        Result := 1
end;
//------------------------------------------------------------------------------
function Storages_Get_DischargeTrigger(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.DischargeTrigger;
end;
//------------------------------------------------------------------------------
function Storages_Get_EffCharge(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.pctChargeEff;
end;
//------------------------------------------------------------------------------
function Storages_Get_EffDischarge(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.pctDischargeEff;
end;
//------------------------------------------------------------------------------
function Storages_Get_Kp(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.dynVars.kP * 1000;
end;
//------------------------------------------------------------------------------
function Storages_Get_kV(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.StorageVars.kVStorageBase;
end;
//------------------------------------------------------------------------------
function Storages_Get_kVA(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.StorageVars.FkVArating;
end;
//------------------------------------------------------------------------------
function Storages_Get_kvar(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.kvarRequested;
end;
//------------------------------------------------------------------------------
function Storages_Get_kVDC(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.dynVars.RatedVDC / 1000;
end;
//------------------------------------------------------------------------------
function Storages_Get_kW(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.kW;
end;
//------------------------------------------------------------------------------
function Storages_Get_kWhRated(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.StorageVars.kWhrating;
end;
//------------------------------------------------------------------------------
function Storages_Get_kWRated(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.StorageVars.kWrating;
end;
//------------------------------------------------------------------------------
function Storages_Get_LimitCurrent(): TAPIBoolean; CDECL;
var
    elem: TStorageObj;
begin
    Result := false;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.CurrentLimited then
        Result := true;
end;
//------------------------------------------------------------------------------
function Storages_Get_PF(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PFnominal;
end;
//------------------------------------------------------------------------------
function Storages_Get_PITol(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.dynVars.CtrlTol * 100;
end;
//------------------------------------------------------------------------------
function Storages_Get_SafeMode(): Integer; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.dynVars.SafeMode then
        Result := 1;
end;
//------------------------------------------------------------------------------
function Storages_Get_SafeVoltage(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.dynVars.SMThreshold;
end;
//------------------------------------------------------------------------------
function Storages_Get_TimeChargeTrig(): Double; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.ChargeTime;
end;
//------------------------------------------------------------------------------
function Storages_Get_VarFollowInverter(): Integer; CDECL;
var
    elem: TStorageObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.VarFollowInverter then
        Result := 1;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_AmpLimit(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.AmpLimit), Value, []);
        Exit;
    end;
    elem.dynVars.ILimit := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_AmpLimitGain(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.AmpLimitGain), Value, []);
        Exit;
    end;
    elem.dynVars.VError := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_ChargeTrigger(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.ChargeTrigger), Value, []);
        Exit;
    end;
    elem.ChargeTrigger := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_ControlMode(Value: Integer); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetInteger(ord(TStorageProp.ControlMode), Value, []);
        Exit;
    end;
    elem.GFM_mode := (Value <> 0);
end;
//------------------------------------------------------------------------------
procedure Storages_Set_DischargeTrigger(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.DischargeTrigger), Value, []);
        Exit;
    end;
    elem.DischargeTrigger := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_EffCharge(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.pctEffCharge), Value, []);
        Exit;
    end;
    elem.pctChargeEff := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_EffDischarge(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.pctEffDischarge), Value, []);
        Exit;
    end;
    elem.pctDischargeEff := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_Kp(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kP), Value, []);
        Exit;
    end;
    elem.dynVars.kP := Value / 1000;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_kV(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kV), Value, []);
        Exit;
    end;
    elem.ParentClass.SetObjDouble(elem, ord(TStorageProp.kV), Value, []);
end;
//------------------------------------------------------------------------------
procedure Storages_Set_kVA(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kVA), Value, []);
        Exit;
    end;
    elem.StorageVars.FkVArating := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_kvar(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kvar), Value, []);
        Exit;
    end;
    elem.kvarRequested := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_kVDC(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kVDC), Value, []);
        Exit;
    end;
    elem.dynVars.RatedVDC := Value * 1000;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_kW(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kW), Value, []);
        Exit;
    end;
    elem.SetkW(Value);
end;
//------------------------------------------------------------------------------
procedure Storages_Set_kWhRated(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kWhrated), Value, []);
        Exit;
    end;
    elem.StorageVars.kWhrating := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_kWRated(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.kWRated), Value, []);
        Exit;
    end;
    elem.StorageVars.kWrating := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_LimitCurrent(Value: TAPIBoolean); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetInteger(ord(TStorageProp.LimitCurrent), Integer(Value), []);
        Exit;
    end;
    elem.CurrentLimited := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_PF(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.pf), Value, []);
        Exit;
    end;
    elem.PFnominal := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_PITol(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.PITol), Value, []);
        Exit;
    end;
    elem.dynVars.CtrlTol := Value / 100;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_SafeVoltage(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.SafeVoltage), Value, []);
        Exit;
    end;
    elem.dynVars.SMThreshold := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_TimeChargeTrig(Value: Double); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetDouble(ord(TStorageProp.TimeChargeTrig), Value, []);
        Exit;
    end;
    elem.ChargeTime := Value;
end;
//------------------------------------------------------------------------------
procedure Storages_Set_VarFollowInverter(Value: Integer); CDECL;
var
    elem: TStorageObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.SetInteger(ord(TStorageProp.VarFollowInverter), Value, []);
        Exit;
    end;
    elem.VarFollowInverter := (Value <> 0);
end;
//------------------------------------------------------------------------------
end.
