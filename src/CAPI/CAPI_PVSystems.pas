unit CAPI_PVSystems;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure PVSystems_Get_AllNames_GR(); CDECL;
procedure PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure PVSystems_Get_RegisterNames_GR(); CDECL;
procedure PVSystems_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure PVSystems_Get_RegisterValues_GR(); CDECL;
function PVSystems_Get_First(): Integer; CDECL;
function PVSystems_Get_Next(): Integer; CDECL;
function PVSystems_Get_Count(): Integer; CDECL;
function PVSystems_Get_idx(): Integer; CDECL;
procedure PVSystems_Set_idx(Value: Integer); CDECL;
function PVSystems_Get_Name(): PAnsiChar; CDECL;
procedure PVSystems_Set_Name(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Irradiance(): Double; CDECL;
procedure PVSystems_Set_Irradiance(Value: Double); CDECL;
function PVSystems_Get_kvar(): Double; CDECL;
function PVSystems_Get_kVArated(): Double; CDECL;
function PVSystems_Get_kW(): Double; CDECL;
function PVSystems_Get_PF(): Double; CDECL;
procedure PVSystems_Set_kVArated(Value: Double); CDECL;
procedure PVSystems_Set_PF(Value: Double); CDECL;
procedure PVSystems_Set_kvar(Value: Double); CDECL;
function PVSystems_Get_Pmpp(): Double; CDECL;
procedure PVSystems_Set_Pmpp(Value: Double); CDECL;
function PVSystems_Get_IrradianceNow(): Double; CDECL;
function PVSystems_Get_Sensor(): PAnsiChar; CDECL;

// API Extensions
function PVSystems_Get_daily(): PAnsiChar; CDECL;
procedure PVSystems_Set_daily(const Value: PAnsiChar); CDECL;
function PVSystems_Get_duty(): PAnsiChar; CDECL;
procedure PVSystems_Set_duty(const Value: PAnsiChar); CDECL;
function PVSystems_Get_yearly(): PAnsiChar; CDECL;
procedure PVSystems_Set_yearly(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Tdaily(): PAnsiChar; CDECL;
procedure PVSystems_Set_Tdaily(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Tduty(): PAnsiChar; CDECL;
procedure PVSystems_Set_Tduty(const Value: PAnsiChar); CDECL;
function PVSystems_Get_Tyearly(): PAnsiChar; CDECL;
procedure PVSystems_Set_Tyearly(const Value: PAnsiChar); CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    PVSystem,
    PVSystem2,
    SysUtils,
    PCElement,
    DSSClass,
    DSSHelper;

//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TPVSystemObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.PVSystems.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active PVSystem object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function _activeObj2(DSS: TDSSContext; out obj: TPVSystem2Obj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.PVSystems.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active PVSystem object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.PVSystems, False);
end;

procedure PVSystems_Get_AllNames_GR(); CDECL;
// Same as PVSystems_Get_AllNames but uses global result (GR) pointers
begin
    PVSystems_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    k: Integer;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumPVSystemRegisters);
        for k := 0 to NumPVSystemRegisters - 1 do
        begin
            Result[k] := DSS_CopyStringAsPChar(DSSPrime.PVSystemClass.RegisterNames[k + 1]);
        end;
        Exit;
    end;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumPVSystem2Registers);
    for k := 0 to NumPVSystem2Registers - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(DSSPrime.PVSystem2Class.RegisterNames[k + 1]);
    end;
end;

procedure PVSystems_Get_RegisterNames_GR(); CDECL;
// Same as PVSystems_Get_RegisterNames but uses global result (GR) pointers
begin
    PVSystems_Get_RegisterNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
procedure PVSystems_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    PVSystem: TPVSystemObj;
    PVSystem2: TPVSystem2Obj;
    k: Integer;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, pVSystem) then
        begin
            DefaultResult(ResultPtr, ResultCount);
            Exit;
        end;
            
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, numPVSystemRegisters);
        for k := 0 to numPVSystemRegisters - 1 do
        begin
            Result[k] := PVSystem.Registers[k + 1];
        end;
        Exit;
    end;
    
    if not _activeObj2(DSSPrime, PVSystem2) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
        
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, numPVSystem2Registers);
    for k := 0 to numPVSystem2Registers - 1 do
    begin
        Result[k] := PVSystem2.Registers[k + 1];
    end;
    
end;

procedure PVSystems_Get_RegisterValues_GR(); CDECL;
// Same as PVSystems_Get_RegisterValues but uses global result (GR) pointers
begin
    PVSystems_Get_RegisterValues(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
function PVSystems_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.PVSystems);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.PVSystems);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;

    Result := DSSPrime.ActiveCircuit.PVSystems.Count;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.PVSystems.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_idx(Value: Integer); CDECL;
var
    pPVSystem: TPCElement;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    pPVSystem := DSSPrime.ActiveCircuit.PVSystems.Get(Value);
    if pPVSystem = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid PVSystem index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pPVSystem;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Name(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSS_CAPI_LEGACY_MODELS then
    begin
        if DSSPrime.PVSystemClass.SetActive(Value) then
        begin
            DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.PVSystemClass.ElementList.Active;
            DSSPrime.ActiveCircuit.PVSystems.Get(DSSPrime.PVSystemClass.Active);
        end
        else
        begin
            DoSimpleMsg(DSSPrime, 'PVSystem "' + Value + '" Not Found in Active Circuit.', 5003);
        end;
        Exit;
    end;

    if DSSPrime.PVSystem2Class.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.PVSystem2Class.ElementList.Active;
        DSSPrime.ActiveCircuit.PVSystems.Get(DSSPrime.PVSystem2Class.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'PVSystem "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Irradiance(): Double; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := -1.0;  // not set
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := elem.PVSystemVars.FIrradiance;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := elem2.PVSystemVars.FIrradiance;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Irradiance(Value: Double); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.PVSystemVars.FIrradiance := Value;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.PVSystemVars.FIrradiance := Value;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kvar(): Double; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := 0.0;  // not set
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := elem.Presentkvar;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := elem2.Presentkvar;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kVArated(): Double; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := -1.0;  // not set
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := elem.kVARating;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := elem2.kVARating;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kW(): Double; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := 0.0;  // not set
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := elem.PresentkW;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := elem2.PresentkW;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_PF(): Double; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := 0.0;  // not set
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := elem.PowerFactor;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := elem2.PowerFactor;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_kVArated(Value: Double); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.kVARating := Value;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.kVARating := Value;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_PF(Value: Double); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.Varmode := 0;
        elem.PowerFactor := Value;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.Varmode := 0;
    elem2.PowerFactor := Value;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_kvar(Value: Double); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.Varmode := VARMODEKVAR;
        elem.Presentkvar := Value;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.Varmode := VARMODEKVAR;
    elem2.Presentkvar := Value;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_daily(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DailyShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.DailyShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_daily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.DailyShape := Value;
        elem.DailyShapeObj := DSSPrime.LoadShapeClass.Find(elem.DailyShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.DailyShape := Value;
    elem2.DailyShapeObj := DSSPrime.LoadShapeClass.Find(elem2.DailyShape);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_duty(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DutyShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.DutyShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_duty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.DutyShape := Value;
        elem.DutyShapeObj := DSSPrime.LoadShapeClass.Find(elem.DutyShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.DutyShape := Value;
    elem2.DutyShapeObj := DSSPrime.LoadShapeClass.Find(elem2.DutyShape);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_yearly(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.YearlyShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.YearlyShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_yearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.YearlyShape := Value;
        elem.YearlyShapeObj := DSSPrime.LoadShapeClass.Find(elem.YearlyShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.YearlyShape := Value;
    elem2.YearlyShapeObj := DSSPrime.LoadShapeClass.Find(elem2.YearlyShape);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tdaily(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DailyTShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.DailyTShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tdaily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.DailyTShape := Value;
        elem.DailyTShapeObj := DSSPrime.TShapeClass.Find(elem.DailyTShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.DailyTShape := Value;
    elem2.DailyTShapeObj := DSSPrime.TShapeClass.Find(elem2.DailyTShape);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tduty(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DutyTShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.DutyTShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tduty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.DutyTShape := Value;
        elem.DutyTShapeObj := DSSPrime.TShapeClass.Find(elem.DutyTShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.DutyTShape := Value;
    elem2.DutyTShapeObj := DSSPrime.TShapeClass.Find(elem2.DutyTShape);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tyearly(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.YearlyTShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.YearlyTShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tyearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.YearlyTShape := Value;
        elem.YearlyTShapeObj := DSSPrime.TShapeClass.Find(elem.YearlyTShape);
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.YearlyTShape := Value;
    elem2.YearlyTShapeObj := DSSPrime.TShapeClass.Find(elem2.YearlyTShape);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Pmpp(): Double; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := -1.0;  // not set
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := elem.pmpp;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := elem2.pmpp;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Pmpp(Value: Double); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        elem.pmpp := Value;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    elem2.pmpp := Value;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_IrradianceNow(): Double; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := -1.0;  // not set
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        Result := elem.IrradianceNow;
        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;
    Result := elem2.IrradianceNow;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Sensor(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    Result := NIL;
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(DSSPrime, elem) then
            Exit;
        if elem.SensorObj <> NIL then
            Result := DSS_GetAsPAnsiChar(DSSPrime, elem.SensorObj.ElementName);

        Exit;
    end;
    if not _activeObj2(DSSPrime, elem2) then
        Exit;

    if elem2.SensorObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem2.SensorObj.ElementName);
end;
//------------------------------------------------------------------------------
end.
