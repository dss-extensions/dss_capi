unit CAPI_PVSystems;

interface

uses
    CAPI_Utils;

procedure PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure PVSystems_Get_AllNames_GR(); CDECL;
procedure PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
procedure PVSystems_Get_RegisterNames_GR(); CDECL;
procedure PVSystems_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
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
    PCElement;

//------------------------------------------------------------------------------
function _activeObj(out obj: TPVSystemObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.PVSystems.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active PVSystem object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
function _activeObj2(out obj: TPVSystem2Obj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit then
        Exit;
    
    obj := ActiveCircuit.PVSystems.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg('No active PVSystem object found! Activate one and retry.', 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if InvalidCircuit then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.PVSystems, False);
end;

procedure PVSystems_Get_AllNames_GR(); CDECL;
// Same as PVSystems_Get_AllNames but uses global result (GR) pointers
begin
    PVSystems_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure PVSystems_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    k: Integer;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumPVSystemRegisters);
        for k := 0 to NumPVSystemRegisters - 1 do
        begin
            Result[k] := DSS_CopyStringAsPChar(PVSystemClass.RegisterNames[k + 1]);
        end;
        Exit;
    end;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumPVSystem2Registers);
    for k := 0 to NumPVSystem2Registers - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(PVSystem2Class.RegisterNames[k + 1]);
    end;
end;

procedure PVSystems_Get_RegisterNames_GR(); CDECL;
// Same as PVSystems_Get_RegisterNames but uses global result (GR) pointers
begin
    PVSystems_Get_RegisterNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
procedure PVSystems_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    PVSystem: TPVSystemObj;
    PVSystem2: TPVSystem2Obj;
    k: Integer;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(PVSystem) then
        begin
            DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
            Exit;
        end;
            
        Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, numPVSystemRegisters);
        for k := 0 to numPVSystemRegisters - 1 do
        begin
            Result[k] := PVSystem.Registers[k + 1];
        end;
        Exit;
    end;
    
    if not _activeObj2(PVSystem2) then
    begin
        DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
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
    PVSystems_Get_RegisterValues(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
function PVSystems_Get_First(): Integer; CDECL;
var
    pPVSystem: TPCElement;
begin
    Result := 0;
    if (InvalidCircuit) or (ActiveCircuit.pVSystems.Count = 0) then
        Exit;
        
    pPVSystem := ActiveCircuit.pVSystems.First;
    repeat
        if pPVSystem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pPVSystem;
            Result := 1;
        end
        else
            pPVSystem := ActiveCircuit.pVSystems.Next;
    until (Result = 1) or (pPVSystem = NIL);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Next(): Integer; CDECL;
var
    pPVSystem: TPCElement;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    
    pPVSystem := ActiveCircuit.PVSystems.Next;
    if pPVSystem = NIL then
        Exit;
        
    repeat
        if pPVSystem.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pPVSystem;
            Result := ActiveCircuit.PVSystems.ActiveIndex;
        end
        else
            pPVSystem := ActiveCircuit.PVSystems.Next;
    until (Result > 0) or (pPVSystem = NIL);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;

    Result := ActiveCircuit.PVSystems.Count;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit then
        Exit;
    Result := ActiveCircuit.PVSystems.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_idx(Value: Integer); CDECL;
var
    pPVSystem: TPCElement;
begin
    if InvalidCircuit then
        Exit;

    pPVSystem := ActiveCircuit.PVSystems.Get(Value);
    if pPVSystem = NIL then
    begin
        DoSimpleMsg('Invalid PVSystem index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pPVSystem;
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
        if not _activeObj(elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(elem.Name);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem2.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit then
        Exit;

    if DSS_CAPI_LEGACY_MODELS then
    begin
        if PVSystemClass.SetActive(Value) then
        begin
            ActiveCircuit.ActiveCktElement := PVSystemClass.ElementList.Active;
            ActiveCircuit.PVSystems.Get(PVSystemClass.Active);
        end
        else
        begin
            DoSimpleMsg('PVSystem "' + Value + '" Not Found in Active Circuit.', 5003);
        end;
        Exit;
    end;

    if PVSystem2Class.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := PVSystem2Class.ElementList.Active;
        ActiveCircuit.PVSystems.Get(PVSystem2Class.Active);
    end
    else
    begin
        DoSimpleMsg('PVSystem "' + Value + '" Not Found in Active Circuit.', 5003);
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
        if not _activeObj(elem) then
            Exit;
        Result := elem.PVSystemVars.FIrradiance;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        elem.PVSystemVars.FIrradiance := Value;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        Result := elem.Presentkvar;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        Result := elem.kVARating;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        Result := elem.PresentkW;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        Result := elem.PowerFactor;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        elem.kVARating := Value;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        elem.Varmode := 0;
        elem.PowerFactor := Value;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        elem.Varmode := VARMODEKVAR;
        elem.Presentkvar := Value;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(elem.DailyShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem2.DailyShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_daily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(elem) then
            Exit;
        elem.DailyShape := Value;
        elem.DailyShapeObj := LoadShapeClass.Find(elem.DailyShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    elem2.DailyShape := Value;
    elem2.DailyShapeObj := LoadShapeClass.Find(elem2.DailyShape);
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
        if not _activeObj(elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(elem.DutyShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem2.DutyShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_duty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(elem) then
            Exit;
        elem.DutyShape := Value;
        elem.DutyShapeObj := LoadShapeClass.Find(elem.DutyShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    elem2.DutyShape := Value;
    elem2.DutyShapeObj := LoadShapeClass.Find(elem2.DutyShape);
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
        if not _activeObj(elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(elem.YearlyShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem2.YearlyShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_yearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(elem) then
            Exit;
        elem.YearlyShape := Value;
        elem.YearlyShapeObj := LoadShapeClass.Find(elem.YearlyShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    elem2.YearlyShape := Value;
    elem2.YearlyShapeObj := LoadShapeClass.Find(elem2.YearlyShape);
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
        if not _activeObj(elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(elem.DailyTShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem2.DailyTShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tdaily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(elem) then
            Exit;
        elem.DailyTShape := Value;
        elem.DailyTShapeObj := TShapeClass.Find(elem.DailyTShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    elem2.DailyTShape := Value;
    elem2.DailyTShapeObj := TShapeClass.Find(elem2.DailyTShape);
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
        if not _activeObj(elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(elem.DutyTShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem2.DutyTShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tduty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(elem) then
            Exit;
        elem.DutyTShape := Value;
        elem.DutyTShapeObj := TShapeClass.Find(elem.DutyTShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    elem2.DutyTShape := Value;
    elem2.DutyTShapeObj := TShapeClass.Find(elem2.DutyTShape);
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
        if not _activeObj(elem) then
            Exit;
        Result := DSS_GetAsPAnsiChar(elem.YearlyTShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := DSS_GetAsPAnsiChar(elem2.YearlyTShape);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tyearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
    elem2: TPVSystem2Obj;
begin
    if DSS_CAPI_LEGACY_MODELS then
    begin
        if not _activeObj(elem) then
            Exit;
        elem.YearlyTShape := Value;
        elem.YearlyTShapeObj := TShapeClass.Find(elem.YearlyTShape);
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    elem2.YearlyTShape := Value;
    elem2.YearlyTShapeObj := TShapeClass.Find(elem2.YearlyTShape);
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
        if not _activeObj(elem) then
            Exit;
        Result := elem.pmpp;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        elem.pmpp := Value;
        Exit;
    end;
    if not _activeObj2(elem2) then
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
        if not _activeObj(elem) then
            Exit;
        Result := elem.IrradianceNow;
        Exit;
    end;
    if not _activeObj2(elem2) then
        Exit;
    Result := elem2.IrradianceNow;
end;
//------------------------------------------------------------------------------
end.
