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

function PVSystems_Get_Pointer(): Pointer; CDECL;

implementation

uses
    CAPI_Constants,
    DSSGlobals,
    PVSystem,
    SysUtils,
    PCElement,
    DSSClass,
    DSSHelper;


function ErrorIfTShapeNil(DSS: TDSSContext; name: String): Pointer;
begin
    Result := DSS.TShapeClass.Find(name);
    if (Result = NIL) and (DSS_CAPI_EXT_ERRORS) then
        DoSimpleMsg(DSS, 'TShape "%s" not found!', [name], 89891);
end;

function ErrorIfLoadShapeNil(DSS: TDSSContext; name: String): Pointer;
begin
    Result := DSS.LoadShapeClass.Find(name);
    if (Result = NIL) and (DSS_CAPI_EXT_ERRORS) then
        DoSimpleMsg(DSS, 'LoadShape "%s" not found!', [name], 89891);
end;

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
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['PVSystem'], 8989);
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
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumPVSystemRegisters);
    for k := 0 to NumPVSystemRegisters - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(DSSPrime.PVSystemClass.RegisterNames[k + 1]);
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
    k: Integer;
begin
    if not _activeObj(DSSPrime, PVSystem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
        
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, numPVSystemRegisters);
    for k := 0 to numPVSystemRegisters - 1 do
    begin
        Result[k] := PVSystem.Registers[k + 1];
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
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['PVSystem', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pPVSystem;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Name(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;

    if DSSPrime.PVSystemClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.PVSystemClass.ElementList.Active;
        DSSPrime.ActiveCircuit.PVSystems.Get(DSSPrime.PVSystemClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'PVSystem "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Irradiance(): Double; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := -1.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PVSystemVars.FIrradiance;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Irradiance(Value: Double); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.PVSystemVars.FIrradiance := Value;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kvar(): Double; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := 0.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Presentkvar;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kVArated(): Double; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := -1.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.kVARating;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_kW(): Double; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := 0.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PresentkW;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_PF(): Double; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := 0.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.PowerFactor;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_kVArated(Value: Double); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.kVARating := Value;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_PF(Value: Double); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Varmode := 0;
    elem.PowerFactor := Value;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_kvar(Value: Double); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.Varmode := VARMODEKVAR;
    elem.Presentkvar := Value;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_daily(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.DailyShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DailyShapeObj.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_daily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.DailyShapeObj := ErrorIfLoadShapeNil(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_duty(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.DutyShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DutyShapeObj.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_duty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.DutyShapeObj := ErrorIfLoadShapeNil(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_yearly(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.YearlyShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.YearlyShapeObj.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_yearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.YearlyShapeObj := ErrorIfLoadShapeNil(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tdaily(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.DailyTShapeObj <> NIL then        
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DailyTShapeObj.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tdaily(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.DailyTShapeObj := ErrorIfTShapeNil(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tduty(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.DutyTShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DutyTShapeObj.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tduty(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.DutyTShapeObj := ErrorIfTShapeNil(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Tyearly(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.YearlyTShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.YearlyTShapeObj.Name);
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Tyearly(const Value: PAnsiChar); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.YearlyTShapeObj := ErrorIfTShapeNil(DSSPrime, Value);
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Pmpp(): Double; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := -1.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.pmpp;
end;
//------------------------------------------------------------------------------
procedure PVSystems_Set_Pmpp(Value: Double); CDECL;
var
    elem: TPVSystemObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.pmpp := Value;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_IrradianceNow(): Double; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := -1.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.IrradianceNow;
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Sensor(): PAnsiChar; CDECL;
var
    elem: TPVSystemObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (elem.SensorObj <> NIL) and (elem.SensorObj.MeteredElement <> NIL) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, AnsiLowerCase(elem.SensorObj.MeteredElement.FullName));
end;
//------------------------------------------------------------------------------
function PVSystems_Get_Pointer(): Pointer; CDECL;
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.PVSystems.Active
end;
//------------------------------------------------------------------------------
end.
