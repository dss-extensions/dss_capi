unit CAPI_Loads;

interface

uses
    CAPI_Utils,
    CAPI_Types;

procedure Loads_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
procedure Loads_Get_AllNames_GR(); CDECL;
function Loads_Get_First(): Integer; CDECL;
function Loads_Get_idx(): Integer; CDECL;
function Loads_Get_Name(): PAnsiChar; CDECL;
function Loads_Get_Next(): Integer; CDECL;
procedure Loads_Set_idx(Value: Integer); CDECL;
procedure Loads_Set_Name(const Value: PAnsiChar); CDECL;
function Loads_Get_kV(): Double; CDECL;
function Loads_Get_kvar(): Double; CDECL;
function Loads_Get_kW(): Double; CDECL;
function Loads_Get_PF(): Double; CDECL;
procedure Loads_Set_kV(Value: Double); CDECL;
procedure Loads_Set_kvar(Value: Double); CDECL;
procedure Loads_Set_kW(Value: Double); CDECL;
procedure Loads_Set_PF(Value: Double); CDECL;
function Loads_Get_Count(): Integer; CDECL;
function Loads_Get_AllocationFactor(): Double; CDECL;
function Loads_Get_Cfactor(): Double; CDECL;
function Loads_Get_Class_(): Integer; CDECL;
function Loads_Get_CVRcurve(): PAnsiChar; CDECL;
function Loads_Get_CVRvars(): Double; CDECL;
function Loads_Get_CVRwatts(): Double; CDECL;
function Loads_Get_daily(): PAnsiChar; CDECL;
function Loads_Get_duty(): PAnsiChar; CDECL;
function Loads_Get_Growth(): PAnsiChar; CDECL;
function Loads_Get_IsDelta(): TAPIBoolean; CDECL;
function Loads_Get_kva(): Double; CDECL;
function Loads_Get_kwh(): Double; CDECL;
function Loads_Get_kwhdays(): Double; CDECL;
function Loads_Get_Model(): Integer; CDECL;
function Loads_Get_NumCust(): Integer; CDECL;
function Loads_Get_PctMean(): Double; CDECL;
function Loads_Get_PctStdDev(): Double; CDECL;
function Loads_Get_Rneut(): Double; CDECL;
function Loads_Get_Spectrum(): PAnsiChar; CDECL;
function Loads_Get_Status(): Integer; CDECL;
function Loads_Get_Vmaxpu(): Double; CDECL;
function Loads_Get_Vminemerg(): Double; CDECL;
function Loads_Get_Vminnorm(): Double; CDECL;
function Loads_Get_Vminpu(): Double; CDECL;
function Loads_Get_xfkVA(): Double; CDECL;
function Loads_Get_Xneut(): Double; CDECL;
function Loads_Get_Yearly(): PAnsiChar; CDECL;
procedure Loads_Set_AllocationFactor(Value: Double); CDECL;
procedure Loads_Set_Cfactor(Value: Double); CDECL;
procedure Loads_Set_Class_(Value: Integer); CDECL;
procedure Loads_Set_CVRcurve(const Value: PAnsiChar); CDECL;
procedure Loads_Set_CVRvars(Value: Double); CDECL;
procedure Loads_Set_CVRwatts(Value: Double); CDECL;
procedure Loads_Set_daily(const Value: PAnsiChar); CDECL;
procedure Loads_Set_duty(const Value: PAnsiChar); CDECL;
procedure Loads_Set_Growth(const Value: PAnsiChar); CDECL;
procedure Loads_Set_IsDelta(Value: TAPIBoolean); CDECL;
procedure Loads_Set_kva(Value: Double); CDECL;
procedure Loads_Set_kwh(Value: Double); CDECL;
procedure Loads_Set_kwhdays(Value: Double); CDECL;
procedure Loads_Set_Model(Value: Integer); CDECL;
procedure Loads_Set_NumCust(Value: Integer); CDECL;
procedure Loads_Set_PctMean(Value: Double); CDECL;
procedure Loads_Set_PctStdDev(Value: Double); CDECL;
procedure Loads_Set_Rneut(Value: Double); CDECL;
procedure Loads_Set_Spectrum(const Value: PAnsiChar); CDECL;
procedure Loads_Set_Status(Value: Integer); CDECL;
procedure Loads_Set_Vmaxpu(Value: Double); CDECL;
procedure Loads_Set_Vminemerg(Value: Double); CDECL;
procedure Loads_Set_Vminnorm(Value: Double); CDECL;
procedure Loads_Set_Vminpu(Value: Double); CDECL;
procedure Loads_Set_xfkVA(Value: Double); CDECL;
procedure Loads_Set_Xneut(Value: Double); CDECL;
procedure Loads_Set_Yearly(const Value: PAnsiChar); CDECL;
procedure Loads_Get_ZIPV(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
procedure Loads_Get_ZIPV_GR(); CDECL;
procedure Loads_Set_ZIPV(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
function Loads_Get_pctSeriesRL(): Double; CDECL;
procedure Loads_Set_pctSeriesRL(Value: Double); CDECL;
function Loads_Get_RelWeight(): Double; CDECL;
procedure Loads_Set_RelWeight(Value: Double); CDECL;
function Loads_Get_Sensor(): PAnsiChar; CDECL;

// API extensions
function Loads_Get_Phases(): Integer; CDECL;
procedure Loads_Set_Phases(Value: Integer); CDECL;
function Loads_Get_Bus1(): PAnsiChar; CDECL;
procedure Loads_Set_Bus1(const Value: PAnsiChar); CDECL;


implementation

uses
    CAPI_Constants,
    DSSGlobals,
    DSSClass,
    DSSHelper,
    Executive,
    Load,
    SysUtils,
    math,
    DSSObjectHelper,
    PCClass;
type
    TObj = TLoadObj;
//---------------------------------------------------------
function _activeObj(DSS: TDSSContext; out obj: TObj): Boolean; inline;
begin
    Result := False;
    obj := NIL;
    if InvalidCircuit(DSS) then
        Exit;
    
    obj := DSS.ActiveCircuit.Loads.Active;
    if obj = NIL then
    begin
        if DSS_CAPI_EXT_ERRORS then
        begin
            DoSimpleMsg(DSS, 'No active %s object found! Activate one and retry.', ['Load'], 8989);
        end;
        Exit;
    end;
    
    Result := True;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: String); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.ParsePropertyValue(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Double); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetDouble(idx, val);
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(DSS: TDSSContext; const idx: Integer; const val: Integer); overload;
var
    elem: TObj;
begin
    if not _activeObj(DSS, elem) then
        Exit;
    DSS.SolutionAbort := FALSE;  // Reset for commands entered from outside
    elem.SetInteger(idx, val);
end;
//------------------------------------------------------------------------------
procedure Loads_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PAPISize); CDECL;
begin
    DefaultResult(ResultPtr, ResultCount);
    if InvalidCircuit(DSSPrime) then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, DSSPrime.ActiveCircuit.Loads, False);
end;

procedure Loads_Get_AllNames_GR(); CDECL;
// Same as Loads_Get_AllNames but uses global result (GR) pointers
begin
    Loads_Get_AllNames(DSSPrime.GR_DataPtr_PPAnsiChar, @DSSPrime.GR_Counts_PPAnsiChar[0])
end;

//------------------------------------------------------------------------------
function Loads_Get_First(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_First(DSSPrime, DSSPrime.ActiveCircuit.Loads);
end;
//------------------------------------------------------------------------------
function Loads_Get_Next(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := Generic_CktElement_Get_Next(DSSPrime, DSSPrime.ActiveCircuit.Loads);
end;
//------------------------------------------------------------------------------
function Loads_Get_idx(): Integer; CDECL;
begin
    Result := 0;
    if InvalidCircuit(DSSPrime) then
        Exit;
    Result := DSSPrime.ActiveCircuit.Loads.ActiveIndex
end;
//------------------------------------------------------------------------------
procedure Loads_Set_idx(Value: Integer); CDECL;
var
    pLoad: TObj;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    pLoad := DSSPrime.ActiveCircuit.Loads.Get(Value);
    if pLoad = NIL then
    begin
        DoSimpleMsg(DSSPrime, 'Invalid %s index: "%d".', ['Load', Value], 656565);
        Exit;
    end;
    DSSPrime.ActiveCircuit.ActiveCktElement := pLoad;
end;
//------------------------------------------------------------------------------
function Loads_Get_Name(): PAnsiChar; CDECL;
var
    pLoad: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
        
    Result := DSS_GetAsPAnsiChar(DSSPrime, pLoad.Name);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if InvalidCircuit(DSSPrime) then
        Exit;
    if DSSPrime.LoadClass.SetActive(Value) then
    begin
        DSSPrime.ActiveCircuit.ActiveCktElement := DSSPrime.LoadClass.ElementList.Active;
        DSSPrime.ActiveCircuit.Loads.Get(DSSPrime.LoadClass.Active);
    end
    else
    begin
        DoSimpleMsg(DSSPrime, 'Load "%s" not found in Active Circuit.', [Value], 5003);
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_kV(): Double; CDECL;
var
    pLoad: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
                
    Result := pLoad.kVLoadBase;
end;
//------------------------------------------------------------------------------
function Loads_Get_kvar(): Double; CDECL;
var
    pLoad: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
                
    Result := pLoad.kvarBase;
end;
//------------------------------------------------------------------------------
function Loads_Get_kW(): Double; CDECL;
var
    pLoad: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
                
    Result := pLoad.kWBase;
end;
//------------------------------------------------------------------------------
function Loads_Get_PF(): Double; CDECL;
var
    pLoad: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
                
    Result := pLoad.PFNominal;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kV(Value: Double); CDECL;
var
    pLoad: TObj;
begin
    if not _activeObj(DSSPrime, pLoad) then
        Exit;

    pLoad.kVLoadBase := Value;
    pload.PropertySideEffects(ord(TLoadProp.kV));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kvar(Value: Double); CDECL;
var
    pLoad: TObj;
begin
    if not _activeObj(DSSPrime, pLoad) then
        Exit;

    pLoad.kvarBase := Value;
    pLoad.LoadSpecType := TLoadSpec.kW_kvar;
    pLoad.RecalcElementData;  // set power factor based on kW, kvar
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kW(Value: Double); CDECL;
var
    pLoad: TObj;
begin
    if not _activeObj(DSSPrime, pLoad) then
        Exit;

    pLoad.kWBase := Value;
    pLoad.LoadSpecType := TLoadSpec.kW_PF;
    pLoad.RecalcElementData; // sets kvar based on kW and pF
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PF(Value: Double); CDECL;
var
    pLoad: TObj;
begin
    if not _activeObj(DSSPrime, pLoad) then
        Exit;

    pLoad.PFNominal := Value;
    pLoad.LoadSpecType := TLoadSpec.kW_PF;
    pLoad.RecalcElementData; //  sets kvar based on kW and pF
end;
//------------------------------------------------------------------------------
function Loads_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(DSSPrime.ActiveCircuit) then
        Result := DSSPrime.ActiveCircuit.Loads.Count;
end;
//------------------------------------------------------------------------------
function Loads_Get_AllocationFactor(): Double; CDECL;
var
    pLoad: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;

    Result := pLoad.AllocationFactor;
end;
//------------------------------------------------------------------------------
function Loads_Get_Cfactor(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.CFactor;
end;
//------------------------------------------------------------------------------
function Loads_Get_Class_(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.LoadClass;
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRcurve(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.CVRshapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.CVRshapeObj.Name);
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRvars(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.CVRvars;
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRwatts(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    Result := elem.CVRwatts;
end;
//------------------------------------------------------------------------------
function Loads_Get_daily(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.DailyShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.DailyShapeObj.Name);
end;
//------------------------------------------------------------------------------
function Loads_Get_duty(): PAnsiChar; CDECL;
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
function Loads_Get_Growth(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.GrowthShapeObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.GrowthShapeObj.Name);
end;
//------------------------------------------------------------------------------
function Loads_Get_IsDelta(): TAPIBoolean; CDECL;
var
    elem: TObj;
begin
    Result := FALSE;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := (elem.Connection = TLoadConnection.Delta);
end;
//------------------------------------------------------------------------------
function Loads_Get_kva(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.kVABase;
end;
//------------------------------------------------------------------------------
function Loads_Get_kwh(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.kWh;
end;
//------------------------------------------------------------------------------
function Loads_Get_kwhdays(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.kWhDays;
end;
//------------------------------------------------------------------------------
function Loads_Get_Model(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := dssLoadConstPQ;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    case elem.FLoadModel of
        TLoadModel.ConstPQ:
            Result := dssLoadConstPQ;
        TLoadModel.ConstZ:
            Result := dssLoadConstZ;
        TLoadModel.Motor:
            Result := dssLoadMotor;
        TLoadModel.CVR:
            Result := dssLoadCVR;
        TLoadModel.ConstI:
            Result := dssLoadConstI;
        TLoadModel.ConstPFixedQ:
            Result := dssLoadConstPFixedQ;
        TLoadModel.ConstPFixedX:
            Result := dssLoadConstPFixedX;
        TLoadModel.ZIPV:
            Result := dssLoadZIPV;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_NumCust(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.NumCustomers;
end;
//------------------------------------------------------------------------------
function Loads_Get_PctMean(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.puMean * 100.0;
end;
//------------------------------------------------------------------------------
function Loads_Get_PctStdDev(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.puStdDev * 100.0;
end;
//------------------------------------------------------------------------------
function Loads_Get_Rneut(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Rneut;
end;
//------------------------------------------------------------------------------
function Loads_Get_Spectrum(): PAnsiChar; CDECL;
var
    elem: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if elem.SpectrumObj <> NIL then
        Result := DSS_GetAsPAnsiChar(DSSPrime, elem.SpectrumObj.Name);
end;
//------------------------------------------------------------------------------
function Loads_Get_Status(): Integer; CDECL;
var
    elem: TObj;
begin
    Result := dssLoadVariable;
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if elem.status = TLoadStatus.Exempt then
        Result := dssLoadExempt
    else if elem.status = TLoadStatus.Fixed then
        Result := dssLoadFixed;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vmaxpu(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MaxPU;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminemerg(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MinEmerg;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminnorm(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MinNormal;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminpu(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.MinPU;
end;
//------------------------------------------------------------------------------
function Loads_Get_xfkVA(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.ConnectedkVA;
end;
//------------------------------------------------------------------------------
function Loads_Get_Xneut(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.Xneut;
end;
//------------------------------------------------------------------------------
function Loads_Get_Yearly(): PAnsiChar; CDECL;
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
procedure Loads_Set_AllocationFactor(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.AllocationFactor), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Cfactor(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.Cfactor), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Class_(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.cls), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRcurve(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.CVRcurve), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRvars(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.CVRvars), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRwatts(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.CVRwatts), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_daily(const Value: PAnsiChar); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.DailyShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    elem.PropertySideEffects(ord(TLoadProp.daily));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_duty(const Value: PAnsiChar); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.DutyShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    elem.PropertySideEffects(ord(TLoadProp.duty));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Growth(const Value: PAnsiChar); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.GrowthShapeObj := DSSPrime.GrowthShapeClass.Find(Value);
    elem.PropertySideEffects(ord(TLoadProp.growth));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_IsDelta(Value: TAPIBoolean); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    if Value then
        elem.Connection := TLoadConnection.Delta
    else
        elem.Connection := TLoadConnection.Wye;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kva(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.kva), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kwh(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.FkWh := Value;
    elem.PropertySideEffects(ord(TLoadProp.kwh));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kwhdays(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.kwhdays), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Model(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;

    if (Value >= Ord(Low(TLoadModel))) and (Value <= Ord(High(TLoadModel))) then
        elem.FLoadModel := TLoadModel(Value)
    else
        DoSimpleMsg(DSSPrime, 'Invalid load model (%d).', [Value], 5004);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_NumCust(Value: Integer); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.NumCust), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PctMean(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.pctmean), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PctStdDev(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.pctstddev), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Rneut(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.Rneut), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Spectrum(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter(DSSPrime, ord(High(TLoadProp)) + ord(TPCElementProp.Spectrum), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Status(Value: Integer); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    case Value of
        dssLoadVariable:
            elem.status := TLoadStatus.Variable;
        dssLoadFixed:
            elem.status := TLoadStatus.Fixed;
        dssLoadExempt:
            elem.status := TLoadStatus.Exempt;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vmaxpu(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.VmaxPu), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminemerg(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.VminEmerg), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminnorm(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.VminNorm), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminpu(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.VminPu), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_xfkVA(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.XfKVA), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Xneut(Value: Double); CDECL;
begin
    Set_Parameter(DSSPrime, ord(TLoadProp.Xneut), Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Yearly(const Value: PAnsiChar); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.YearlyShapeObj := DSSPrime.LoadShapeClass.Find(Value);
    elem.PropertySideEffects(ord(TLoadProp.yearly));
end;
//------------------------------------------------------------------------------
procedure Loads_Get_ZIPV(var ResultPtr: PDouble; ResultCount: PAPISize); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
    begin
        DefaultResult(ResultPtr, ResultCount);
        Exit;
    end;
    DSS_RecreateArray_PDouble(ResultPtr, ResultCount, elem.nZIPV);
    Move(elem.ZipV[1], ResultPtr^, elem.nZIPV * SizeOf(Double));
end;

procedure Loads_Get_ZIPV_GR(); CDECL;
// Same as Loads_Get_ZIPV but uses global result (GR) pointers
begin
    Loads_Get_ZIPV(DSSPrime.GR_DataPtr_PDouble, @DSSPrime.GR_Counts_PDouble[0])
end;
//------------------------------------------------------------------------------
procedure Loads_Set_ZIPV(ValuePtr: PDouble; ValueCount: TAPISize); CDECL;
var
    elem: TObj;
begin
    if ValueCount <> 7 then
    begin
        DoSimpleMsg(DSSPrime, 'ZIPV requires 7 elements, %d were provided!', [ValueCount], 5890);
        Exit;
    end;

    if not _activeObj(DSSPrime, elem) then
        Exit;

    elem.ZIPVset := True;
    Move(ValuePtr[0], elem.ZIPV[1], elem.nZIPV * SizeOf(Double));
end;
//------------------------------------------------------------------------------
function Loads_Get_pctSeriesRL(): Double; CDECL;
var
    elem: TObj;
begin
    Result := -1.0; // signify  bad request
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.puSeriesRL * 100.0;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_pctSeriesRL(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.puSeriesRL := Value / 100.0;
end;
//------------------------------------------------------------------------------
function Loads_Get_RelWeight(): Double; CDECL;
var
    elem: TObj;
begin
    Result := 0.0;
    if not _activeObj(DSSPrime, elem) then
        Exit;
    Result := elem.RelWeighting;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_RelWeight(Value: Double); CDECL;
var
    elem: TObj;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    elem.RelWeighting := Value;
end;
//------------------------------------------------------------------------------
function Loads_Get_Phases(): Integer; CDECL;
var
    pLoad: TObj;
begin
    Result := 0;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
    Result := pLoad.Nphases;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Phases(Value: Integer); CDECL;
var
    elem: TObj;
    prevVal: Integer;
begin
    if not _activeObj(DSSPrime, elem) then
        Exit;
    
    if Value < 1 then
    begin
        DoSimpleMsg(DSSPrime, '%s: Number of phases must be a positive integer!', [elem.FullName], 6568);
        Exit;
    end;
    if (Value <> elem.NPhases) then
    begin
        prevVal := elem.FNPhases;
        elem.FNPhases := Value;
        elem.PropertySideEffects(ord(TLoadProp.phases), prevVal);
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_Bus1(): PAnsiChar; CDECL;
var
    pLoad: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
    Result := DSS_GetAsPAnsiChar(DSSPrime, pLoad.GetBus(1));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Bus1(const Value: PAnsiChar); CDECL;
var
    pLoad: TObj;
begin
    if not _activeObj(DSSPrime, pLoad) then
        Exit;
    pLoad.SetBus(1, Value);
    pLoad.PropertySideEffects(ord(TLoadProp.bus1));
end;
//------------------------------------------------------------------------------
function Loads_Get_Sensor(): PAnsiChar; CDECL;
var
    pLoad: TObj;
begin
    Result := NIL;
    if not _activeObj(DSSPrime, pLoad) then
        Exit;

    if (pLoad.SensorObj <> NIL) and (pLoad.SensorObj.MeteredElement <> NIL) then
        Result := DSS_GetAsPAnsiChar(DSSPrime, LowerCase(pLoad.SensorObj.MeteredElement.FullName));
end;
//------------------------------------------------------------------------------
end.
