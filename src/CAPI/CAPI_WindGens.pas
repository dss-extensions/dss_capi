unit CAPI_WindGens;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure WindGens_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
function WindGens_Get_First(): Integer; CDECL;
function WindGens_Get_Name(): PAnsiChar; CDECL;
function WindGens_Get_Next(): Integer; CDECL;
procedure WindGens_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure WindGens_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure WindGens_Get_RegisterValues_GR(); CDECL;
procedure WindGens_Set_Name(const Value: PAnsiChar); CDECL;
function WindGens_Get_Count(): Integer; CDECL;
function WindGens_Get_idx(): Integer; CDECL;
procedure WindGens_Set_idx(Value: Integer); CDECL;
procedure WindGens_Set_PF(Value: Double); CDECL;
function WindGens_Get_PF(): Double; CDECL;
procedure WindGens_Set_kvar(Value: Double); CDECL;
function WindGens_Get_kvar(): Double; CDECL;
procedure WindGens_Set_kW(Value: Double); CDECL;
function WindGens_Get_kW(): Double; CDECL;
procedure WindGens_Set_kV(Value: Double); CDECL;
function WindGens_Get_kV(): Double; CDECL;
function WindGens_Get_kVA(): Double; CDECL;
procedure WindGens_Set_kVA(Value: Double); CDECL;
function WindGens_Get_Ag(): Double; CDECL;
function WindGens_Get_Cp(): Double; CDECL;
function WindGens_Get_Lamda(): Double; CDECL;
function WindGens_Get_N_WTG(): Integer; CDECL;
function WindGens_Get_NPoles(): Integer; CDECL;
function WindGens_Get_pd(): Double; CDECL;
function WindGens_Get_PSS(): Double; CDECL;
function WindGens_Get_QFlag(): Integer; CDECL;
function WindGens_Get_QMode(): Integer; CDECL;
function WindGens_Get_QSS(): Double; CDECL;
function WindGens_Get_Rad(): Double; CDECL;
function WindGens_Get_RThev(): Double; CDECL;
function WindGens_Get_VCutIn(): Double; CDECL;
function WindGens_Get_VCutOut(): Double; CDECL;
function WindGens_Get_Vss(): Double; CDECL;
function WindGens_Get_WindSpeed(): Double; CDECL;
function WindGens_Get_XThev(): Double; CDECL;
procedure WindGens_Set_Ag(Value: Double); CDECL;
procedure WindGens_Set_Cp(Value: Double); CDECL;
procedure WindGens_Set_Lamda(Value: Double); CDECL;
procedure WindGens_Set_N_WTG(Value: Integer); CDECL;
procedure WindGens_Set_NPoles(Value: Integer); CDECL;
procedure WindGens_Set_pd(Value: Double); CDECL;
procedure WindGens_Set_PSS(Value: Double); CDECL;
procedure WindGens_Set_QFlag(Value: Integer); CDECL;
procedure WindGens_Set_QMode(Value: Integer); CDECL;
procedure WindGens_Set_QSS(Value: Double); CDECL;
procedure WindGens_Set_Rad(Value: Double); CDECL;
procedure WindGens_Set_RThev(Value: Double); CDECL;
procedure WindGens_Set_VCutIn(Value: Double); CDECL;
procedure WindGens_Set_VCutOut(Value: Double); CDECL;
procedure WindGens_Set_Vss(Value: Double); CDECL;
procedure WindGens_Set_WindSpeed(Value: Double); CDECL;
procedure WindGens_Set_XThev(Value: Double); CDECL;

// API Extensions
function WindGens_Get_Pointer(): Pointer; CDECL;
function WindGens_Get_Phases(): Integer; CDECL;
procedure WindGens_Set_Phases(Value: Integer); CDECL;
function WindGens_Get_Class_(): Integer; CDECL;
procedure WindGens_Set_Class_(Value: Integer); CDECL;
function WindGens_Get_daily(): PAnsiChar; CDECL;
function WindGens_Get_duty(): PAnsiChar; CDECL;
function WindGens_Get_Yearly(): PAnsiChar; CDECL;
procedure WindGens_Set_daily(const Value: PAnsiChar); CDECL;
procedure WindGens_Set_duty(const Value: PAnsiChar); CDECL;
procedure WindGens_Set_Yearly(const Value: PAnsiChar); CDECL;
function WindGens_Get_IsDelta(): TAPIBoolean; CDECL;
procedure WindGens_Set_IsDelta(Value: TAPIBoolean); CDECL;
function WindGens_Get_Bus1(): PAnsiChar; CDECL;
procedure WindGens_Set_Bus1(const Value: PAnsiChar); CDECL;

implementation

uses
    CAPI_Constants,
    DSSCLassDefs,
    DSSGlobals,
    WindGen,
    CktElement,
    SysUtils,
    DSSClass,
    DSSHelper,
    DSSObjectHelper,
    ArrayDef;

type
    TObj = TWindGenObj;
//------------------------------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TWindGenObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.WindGenClass.GetActiveObj;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['WindGen'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure WindGens_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.WindGenClass.ElementList, False);
end;
//------------------------------------------------------------------------------
function WindGens_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.WindGenClass.ElementList);
end;
//------------------------------------------------------------------------------
function WindGens_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.WindGenClass.ElementList);
end;
//------------------------------------------------------------------------------
function WindGens_Get_Name(): PAnsiChar; CDECL;
var
    elem: TWindGenObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.Name());
end;
//------------------------------------------------------------------------------
procedure WindGens_Get_RegisterNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
var
    Result: PPAnsiCharArray0;
    WindGenCls: TWindGen;
    k: Integer;
begin
    WindGenCls := DSSPrime.WindGenClass;
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, NumWGenRegisters);
    for k := 0 to NumWGenRegisters - 1 do
    begin
        Result[k] := DSS_CopyStringAsPChar(WindGenCls.RegisterNames[k]);
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Get_RegisterValues(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    Result: PDoubleArray0;
    Gen: TWindGenObj;
    k: Integer;
begin
    if not _activeObj(DSSPrime, Gen) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;

    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, NumWGenRegisters);
    for k := 0 to NumWGenRegisters - 1 do
    begin
        Result[k] := Gen.Registers[k + 1];
    end;
end;

procedure WindGens_Get_RegisterValues_GR(); CDECL;
// Same as WindGens_Get_RegisterValues but uses global result (GR) pointers
begin
    WindGens_Get_RegisterValues(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;

//------------------------------------------------------------------------------
procedure WindGens_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.WindGenClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.SetActiveCktElement(DSSPrime.WindGenClass.ElementList.Active);
        DSSPrime.WindGenClass.ElementList.Get(DSSPrime.WindGenClass.ActiveIndex());
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'WindGen "%s" not found in Active Circuit.', [Value], 20003);
    end;
end;
//------------------------------------------------------------------------------
function WindGens_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WindGenClass.ElementList.Count;
end;
//------------------------------------------------------------------------------
function WindGens_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WindGenClass.ElementList.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_idx(Value: Integer); CDECL;
var
    elem: TWindGenObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    elem := DSSPrime.WindGenClass.ElementList.Get(Value);
    if elem = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['WindGen', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.SetActiveCktElement(elem);
end;
//------------------------------------------------------------------------------
function WindGens_Get_Pointer(): Pointer; CDECL; // API Extension
begin
    Result := NIL;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.WindGenClass.ElementList.Active
end;
//------------------------------------------------------------------------------
function WindGens_Get_kV(): Double; CDECL;
var
    elem: TWindGenObj;
begin
    Result := -1.0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.kVWindGenBase;
end;
//------------------------------------------------------------------------------
function WindGens_Get_kvar(): Double; CDECL;
var
    elem: TWindGenObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.Qnominalperphase * 0.001 * elem.FNPhases;
end;
//------------------------------------------------------------------------------
function WindGens_Get_kW(): Double; CDECL;
var
    elem: TWindGenObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.Pnominalperphase * 0.001 * elem.FNPhases;
end;
//------------------------------------------------------------------------------
function WindGens_Get_PF(): Double; CDECL;
var
    elem: TWindGenObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.PFNominal;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_kV(Value: Double); CDECL;
var
    elem: TWindGenObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.GenVars.kVWindGenBase := Value;
    elem.PropertySideEffects(ord(TWindGenProp.kV), 0, []);
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_kvar(Value: Double); CDECL;
var
    elem: TWindGenObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.kvarBase := Value;
    elem.PropertySideEffects(ord(TWindGenProp.kvar), 0, []);
    elem.RecalcElementData();
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_kW(Value: Double); CDECL;
var
    elem: TWindGenObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.kWBase := Value;
    elem.SyncUpPowerQuantities();
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_PF(Value: Double); CDECL;
var
    elem: TWindGenObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.PFNominal := Value;
end;
//------------------------------------------------------------------------------
function WindGens_Get_kVA(): Double; CDECL;
var
    elem: TWindGenObj;
begin
    Result := -1.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.Genvars.kVArating;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_kVA(Value: Double); CDECL;
var
    elem: TWindGenObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.Genvars.kVArating := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.PropertySideEffects(ord(TWindGenProp.kVA), 0, []);
        elem.RecalcElementData();
    end
    else
    begin
        elem.WindModelDyn.ratedKVA := Value;
        elem.WindModelDyn.RecalcElementData();
    end;
end;
//------------------------------------------------------------------------------
function WindGens_Get_Phases(): Integer; CDECL; // API Extension
var
    elem: TWindGenObj;
begin
    Result := 0;  // not set
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.nphases;
end;

//------------------------------------------------------------------------------
procedure WindGens_Set_Phases(Value: Integer); CDECL; // API Extension
var
    elem: TWindGenObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if Value < 1 then
    begin
        DoSimpleMsg(DSSPrime, '%s: Number of phases must be a positive integer!', [elem.FullName()], 6568);
        Exit;
    end;
    elem.FNphases := Value;

    // if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    // begin
    elem.PropertySideEffects(ord(TWindGenProp.Phases), 0, []);
    elem.RecalcElementData();
    elem.YPrimInvalid := true;
    // end;
end;

function WindGens_Get_Bus1(): PAnsiChar; CDECL; // API Extension
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, elem.GetBus(1));
end;
//------------------------------------------------------------------------------
function WindGens_Get_daily(): PAnsiChar; CDECL; // API Extension
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.DailyDispShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DailyDispShapeObj.Name);
end;
//------------------------------------------------------------------------------
function WindGens_Get_Class_(): Integer; CDECL; // API Extension
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenClass;
end;
//------------------------------------------------------------------------------
function WindGens_Get_duty(): PAnsiChar; CDECL; // API Extension
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.DutyShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DutyShapeObj.Name);
end;
//------------------------------------------------------------------------------
function WindGens_Get_IsDelta(): TAPIBoolean; CDECL; // API Extension
var
    elem: TObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := (elem.Connection = TGeneralConnection.Delta);
end;
//------------------------------------------------------------------------------
function WindGens_Get_Yearly(): PAnsiChar; CDECL; // API Extension
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.YearlyShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.YearlyShapeObj.Name);
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Bus1(const Value: PAnsiChar); CDECL; // API Extension
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.SetBus(1, Value);
    elem.PropertySideEffects(ord(TWindGenProp.bus1), 0, []);
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Class_(Value: Integer); CDECL; // API Extension
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.SetInteger(ord(TWindGenProp.cls), Value, []);
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_daily(const Value: PAnsiChar); CDECL; // API Extension
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.DailyDispShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    elem.PropertySideEffects(ord(TWindGenProp.daily), 0, []);
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_duty(const Value: PAnsiChar); CDECL; // API Extension
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.DutyShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    elem.PropertySideEffects(ord(TWindGenProp.duty), 0, []);
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_IsDelta(Value: TAPIBoolean); CDECL; // API Extension
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if Value then
        elem.Connection := TGeneralConnection.Delta
    else
        elem.Connection := TGeneralConnection.Wye;

    elem.PropertySideEffects(ord(TWindGenProp.conn), 0, []);
    elem.RecalcElementData();
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Yearly(const Value: PAnsiChar); CDECL; // API Extension
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.YearlyShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    elem.PropertySideEffects(ord(TWindGenProp.yearly), 0, []);
end;
//------------------------------------------------------------------------------
function WindGens_Get_Ag(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.Ag;
end;
//------------------------------------------------------------------------------
function WindGens_Get_Cp(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.Cp;
end;
//------------------------------------------------------------------------------
function WindGens_Get_Lamda(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.Lamda;
end;
//------------------------------------------------------------------------------
function WindGens_Get_N_WTG(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.N_WTG;
end;
//------------------------------------------------------------------------------
function WindGens_Get_NPoles(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := Trunc(elem.GenVars.Poles);
end;
//------------------------------------------------------------------------------
function WindGens_Get_pd(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.pd;
end;
//------------------------------------------------------------------------------
function WindGens_Get_PSS(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.PSS;
end;
//------------------------------------------------------------------------------
function WindGens_Get_QFlag(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.QFlg;
end;
//------------------------------------------------------------------------------
function WindGens_Get_QMode(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.QMode;
end;
//------------------------------------------------------------------------------
function WindGens_Get_QSS(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.QSS;
end;
//------------------------------------------------------------------------------
function WindGens_Get_Rad(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.Rad;
end;
//------------------------------------------------------------------------------
function WindGens_Get_RThev(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.Zthev.re;
end;
//------------------------------------------------------------------------------
function WindGens_Get_VCutIn(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.VCutIn;
end;
//------------------------------------------------------------------------------
function WindGens_Get_VCutOut(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.GenVars.VCutOut;
end;
//------------------------------------------------------------------------------
function WindGens_Get_Vss(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.Vss;
end;
//------------------------------------------------------------------------------
function WindGens_Get_WindSpeed(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.VWind;
end;
//------------------------------------------------------------------------------
function WindGens_Get_XThev(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.WindModelDyn.Zthev.im;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.ReCalcElementData();
        elem.YPrimInvalid := True;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Ag(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.GenVars.Ag := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        // doesn't affect Yprim
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Cp(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    elem.GenVars.Cp := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Lamda(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.GenVars.Lamda := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_N_WTG(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.N_WTG := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_NPoles(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.GenVars.Poles := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_pd(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.GenVars.pd := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_PSS(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.PSS := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        //TODO: do we need to reinit the dyn model?
    end;    
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_QFlag(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.QFlg := Value;
    // No direct side effects, will affect the next solution
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_QMode(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.QMode := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
        //TODO: do we need to reinit the dyn model?
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_QSS(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.QSS := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        //TODO: do we need to reinit the dyn model?
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Rad(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.GenVars.Rad := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_RThev(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.Zthev.re := Value;
    // No direct side effects
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_VCutIn(Value: Double); CDECL;
var
    elem: TObj;
    prev: Double;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prev := elem.GenVars.VCutIn;
    elem.GenVars.VCutIn := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        if ((elem.ShapeFactor.re < prev) <> (elem.ShapeFactor.re < elem.GenVars.VCutin)) then
        begin
            elem.RecalcElementData();
            elem.YPrimInvalid := true;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_VCutOut(Value: Double); CDECL;
var
    elem: TObj;
    prev: Double;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    prev := elem.GenVars.VCutOut;
    elem.GenVars.VCutOut := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        if ((prev > elem.GenVars.VCutout) <> (elem.ShapeFactor.re > elem.GenVars.VCutout)) then
        begin
            elem.RecalcElementData();
            elem.YPrimInvalid := true;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_Vss(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.Vss := Value;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_WindSpeed(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.VWind := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
        elem.YPrimInvalid := true;
    end;
end;
//------------------------------------------------------------------------------
procedure WindGens_Set_XThev(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.WindModelDyn.Zthev.im := Value;
    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.SkipSideEffects)) = 0 then
    begin
        elem.RecalcElementData();
    end;
end;
//------------------------------------------------------------------------------
end.
