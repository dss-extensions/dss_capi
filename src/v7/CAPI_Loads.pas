unit CAPI_Loads;

{$inline on}

interface

uses
    CAPI_Utils;

procedure Loads_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
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
function Loads_Get_IsDelta(): Wordbool; CDECL;
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
procedure Loads_Set_IsDelta(Value: Wordbool); CDECL;
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
procedure Loads_Get_ZIPV(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
procedure Loads_Get_ZIPV_GR(); CDECL;
procedure Loads_Set_ZIPV(ValuePtr: PDouble; ValueCount: Integer); CDECL;
function Loads_Get_pctSeriesRL(): Double; CDECL;
procedure Loads_Set_pctSeriesRL(Value: Double); CDECL;
function Loads_Get_RelWeight(): Double; CDECL;
procedure Loads_Set_RelWeight(Value: Double); CDECL;

// API extensions
function Loads_Get_Phases(): Integer; CDECL;
procedure Loads_Set_Phases(Value: Integer); CDECL;
function Loads_Get_Bus1(): PAnsiChar; CDECL;
procedure Loads_Set_Bus1(const Value: PAnsiChar); CDECL;


implementation

uses
    CAPI_Constants,
    DSSGlobals,
    Executive,
    Load,
    SysUtils,
    math;

type
    LoadProps = (
        phases = 1, bus1 = 2, kV = 3, kW = 4, pf = 5, model = 6, yearly = 7, daily = 8, duty = 9, growth = 10, conn = 11, kvar = 12,
        Rneut = 13, Xneut = 14, status = 15, cls = 16, Vminpu = 17, Vmaxpu = 18, Vminnorm = 19, Vminemerg = 20, xfkVA = 21,
        allocationfactor = 22, kVA = 23, pctmean = 24, pctstddev = 25, CVRwatts = 26, CVRvars = 27, kwh = 28, kwhdays = 29,
        Cfactor = 30, CVRcurve = 31, NumCust = 32, ZIPV = 33, pctSeriesRL = 34, RelWeight = 35, Vlowpu = 36,
        puXharm = 37, XRhar = 38
        );

//------------------------------------------------------------------------------
procedure LoadPropSideEffects(prop: LoadProps; load: TLoadObj); //incomplete
begin
    with load do
    begin
    // << SIDE EFFECTS >>
    // keep kvar nominal up to date WITH kW and PF
        case prop of
            LoadProps.phases:
            begin
            // -> SetNcondsForConnection  // Force Reallocation of terminal info
                case Connection of
                    0:
                        NConds := Fnphases + 1;
                    1:
                        case Fnphases of
                            1, 2:
                                NConds := Fnphases + 1; // L-L and Open-delta
                        else
                            NConds := Fnphases;
                        end;
                else  {nada}
                end;
            // <- SetNcondsForConnection
                UpdateVoltageBases;
            end;

            LoadProps.kV:
                UpdateVoltageBases;

            LoadProps.kW:
                LoadSpecType := 0;

            LoadProps.pf:
            begin
                PFChanged := TRUE;
                PFSpecified := TRUE;
            end;
            {Set shape objects;  returns nil if not valid}
            {Sets the kW and kvar properties to match the peak kW demand from the Loadshape}
            LoadProps.yearly:
            begin
                YearlyShapeObj := LoadShapeClass.Find(YearlyShape);
                if Assigned(YearlyShapeObj) then
                    with YearlyShapeObj do
                        if UseActual then
                            SetkWkvar(MaxP, MaxQ);
            end;
            LoadProps.daily:
            begin
                DailyShapeObj := LoadShapeClass.Find(DailyShape);
                if Assigned(DailyShapeObj) then
                    with DailyShapeObj do
                        if UseActual then
                            SetkWkvar(MaxP, MaxQ);
                {If Yearly load shape is not yet defined, make it the same as Daily}
                if YearlyShapeObj = NIL then
                    YearlyShapeObj := DailyShapeObj;
            end;
            LoadProps.duty:
            begin
                DutyShapeObj := LoadShapeClass.Find(DutyShape);
                if Assigned(DutyShapeObj) then
                    with DutyShapeObj do
                        if UseActual then
                            SetkWkvar(MaxP, MaxQ);
            end;
            LoadProps.growth:
                GrowthShapeObj := GrowthShapeClass.Find(GrowthShape);

            LoadProps.kvar:
            begin
                LoadSpecType := 1;
                PFSpecified := FALSE;
            end;// kW, kvar
 {*** see set_xfkva, etc           21, 22: LoadSpectype := 3;  // XFKVA*AllocationFactor, PF  }
            LoadProps.pctMean:
                LoadSpecType := 2;  // kVA, PF
 {*** see set_kwh, etc           28..30: LoadSpecType := 4;  // kWh, days, cfactor, PF }
            LoadProps.CVRCurve:
                CVRShapeObj := LoadShapeClass.Find(CVRshape);
        end;
    end;
end;
//------------------------------------------------------------------------------
function ActiveLoad: TLoadObj;
begin
    Result := NIL;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Loads.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('load.%s.%s=%s', [ActiveLoad.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Loads_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit = NIL then
        Exit;
    Generic_Get_AllNames(ResultPtr, ResultCount, ActiveCircuit.Loads, False);
end;

procedure Loads_Get_AllNames_GR(); CDECL;
// Same as Loads_Get_AllNames but uses global result (GR) pointers
begin
    Loads_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Loads_Get_First(): Integer; CDECL;
var
    pLoad: TLoadObj;
begin
    Result := 0;
    if ActiveCircuit = NIL then
        Exit;
    pLoad := ActiveCircuit.Loads.First;
    if pLoad = NIL then
        Exit;
    repeat
        if pLoad.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pLoad;
            Result := 1;
        end
        else
            pLoad := ActiveCircuit.Loads.Next;
    until (Result = 1) or (pLoad = NIL);
end;
//------------------------------------------------------------------------------
function Loads_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Loads.ActiveIndex
    else
        Result := 0
end;
//------------------------------------------------------------------------------
procedure Loads_Set_idx(Value: Integer); CDECL;
var
    pLoad: TLoadObj;
begin
    if ActiveCircuit = NIL then
        Exit;
    pLoad := ActiveCircuit.Loads.Get(Value);
    if pLoad = NIL then
    begin
        DoSimpleMsg('Invalid Load index: "' + IntToStr(Value) + '".', 656565);
        Exit;
    end;
    ActiveCircuit.ActiveCktElement := pLoad;
end;
//------------------------------------------------------------------------------
function Loads_Get_Name_AnsiString(): Ansistring; inline;
var
    pLoad: TLoadObj;
begin
    Result := '';
    if ActiveCircuit = NIL then 
        Exit;

    pLoad := ActiveCircuit.Loads.Active;
    if pLoad <> NIL then
        Result := pLoad.Name
    else
        Result := '';  // signify no name
end;

function Loads_Get_Name(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_Next(): Integer; CDECL;
var
    pLoad: TLoadObj;
begin
    Result := 0;
    if ActiveCircuit = NIL then Exit;
    pLoad := ActiveCircuit.Loads.Next;
    if pLoad = NIL then Exit;
    repeat
        if pLoad.Enabled then
        begin
            ActiveCircuit.ActiveCktElement := pLoad;
            Result := ActiveCircuit.Loads.ActiveIndex;
        end
        else
            pLoad := ActiveCircuit.Loads.Next;
    until (Result > 0) or (pLoad = NIL);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Name(const Value: PAnsiChar); CDECL;
begin
    if ActiveCircuit = NIL then
        Exit;
    if LoadClass.SetActive(Value) then
    begin
        ActiveCircuit.ActiveCktElement := LoadClass.ElementList.Active;
        ActiveCircuit.Loads.Get(LoadClass.Active);
    end
    else
    begin
        DoSimpleMsg('Load "' + Value + '" Not Found in Active Circuit.', 5003);
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_kV(): Double; CDECL;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TLoadObj(Active).kVLoadBase;
            end;
        end;
    end;

end;
//------------------------------------------------------------------------------
function Loads_Get_kvar(): Double; CDECL;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TLoadObj(Active).kvarBase;
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_kW(): Double; CDECL;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TLoadObj(Active).kWBase;
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_PF(): Double; CDECL;
begin
    Result := 0.0;
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                Result := TLoadObj(Active).PFNominal;
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kV(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                TLoadObj(Active).kVLoadBase := Value;
                TLoadObj(Active).UpdateVoltageBases;  // side effects
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kvar(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                TLoadObj(Active).kvarBase := Value;
                TLoadObj(Active).LoadSpecType := 1;
                TLoadObj(Active).RecalcElementData;  // set power factor based on kW, kvar
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kW(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                TLoadObj(Active).kWBase := Value;
                TLoadObj(Active).LoadSpecType := 0;
                TLoadObj(Active).RecalcElementData; // sets kvar based on kW and pF
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PF(Value: Double); CDECL;
begin
    if ActiveCircuit <> NIL then
    begin
        with ActiveCircuit.Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                TLoadObj(Active).PFNominal := Value;
                TLoadObj(Active).LoadSpecType := 0;
                TLoadObj(Active).RecalcElementData; //  sets kvar based on kW and pF
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_Count(): Integer; CDECL;
begin
    Result := 0;
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.Loads.ListSize;
end;
//------------------------------------------------------------------------------
function Loads_Get_AllocationFactor(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.AllocationFactor;
end;
//------------------------------------------------------------------------------
function Loads_Get_Cfactor(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CFactor;
end;
//------------------------------------------------------------------------------
function Loads_Get_Class_(): Integer; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.LoadClass;
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRcurve_AnsiString(): Ansistring; inline;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CVRshape;
end;

function Loads_Get_CVRcurve(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_CVRcurve_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRvars(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CVRvars;
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRwatts(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CVRwatts;
end;
//------------------------------------------------------------------------------
function Loads_Get_daily_AnsiString(): Ansistring; inline;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.DailyShape;
end;

function Loads_Get_daily(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_daily_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_duty_AnsiString(): Ansistring; inline;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.DailyShape;
end;

function Loads_Get_duty(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_duty_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_Growth_AnsiString(): Ansistring; inline;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.GrowthShape;
end;

function Loads_Get_Growth(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Growth_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_IsDelta(): Wordbool; CDECL;
var
    elem: TLoadObj;
begin
    Result := FALSE;
    elem := ActiveLoad;
    if elem <> NIL then
        if elem.Connection > 0 then
            Result := TRUE;
end;
//------------------------------------------------------------------------------
function Loads_Get_kva(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.kVABase;
end;
//------------------------------------------------------------------------------
function Loads_Get_kwh(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.kWh;
end;
//------------------------------------------------------------------------------
function Loads_Get_kwhdays(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.kWhDays;
end;
//------------------------------------------------------------------------------
function Loads_Get_Model(): Integer; CDECL;
var
    elem: TLoadObj;
begin
    Result := dssLoadConstPQ;
    elem := ActiveLoad;
    if elem <> NIL then
    begin
        case elem.FLoadModel of
            1:
                Result := dssLoadConstPQ;
            2:
                Result := dssLoadConstZ;
            3:
                Result := dssLoadMotor;
            4:
                Result := dssLoadCVR;
            5:
                Result := dssLoadConstI;
            6:
                Result := dssLoadConstPFixedQ;
            7:
                Result := dssLoadConstPFixedX;
            8:
                Result := dssLoadZIPV;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_NumCust(): Integer; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.NumCustomers;
end;
//------------------------------------------------------------------------------
function Loads_Get_PctMean(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.puMean * 100.0;
end;
//------------------------------------------------------------------------------
function Loads_Get_PctStdDev(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.puStdDev * 100.0;
end;
//------------------------------------------------------------------------------
function Loads_Get_Rneut(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.Rneut;
end;
//------------------------------------------------------------------------------
function Loads_Get_Spectrum_AnsiString(): Ansistring; inline;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.Spectrum;
end;

function Loads_Get_Spectrum(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Spectrum_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_Status(): Integer; CDECL;
var
    elem: TLoadObj;
begin
    Result := dssLoadVariable;
    elem := ActiveLoad;
    if elem <> NIL then
    begin
        if elem.ExemptLoad then
            Result := dssLoadExempt
        else
        if elem.FixedLoad then
            Result := dssLoadFixed;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vmaxpu(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MaxPU;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminemerg(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MinEmerg;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminnorm(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MinNormal;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminpu(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MinPU;
end;
//------------------------------------------------------------------------------
function Loads_Get_xfkVA(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.ConnectedkVA;
end;
//------------------------------------------------------------------------------
function Loads_Get_Xneut(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.Xneut;
end;
//------------------------------------------------------------------------------
function Loads_Get_Yearly_AnsiString(): Ansistring; inline;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.YearlyShape;
end;

function Loads_Get_Yearly(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Yearly_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Loads_Set_AllocationFactor(Value: Double); CDECL;
begin
    Set_Parameter('AllocationFactor', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Cfactor(Value: Double); CDECL;
begin
    Set_Parameter('Cfactor', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Class_(Value: Integer); CDECL;
begin
    Set_Parameter('Class', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRcurve(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('CVRcurve', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRvars(Value: Double); CDECL;
begin
    Set_Parameter('CVRvars', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRwatts(Value: Double); CDECL;
begin
    Set_Parameter('CVRwatts', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_daily(const Value: PAnsiChar); CDECL;
var
    elem: TLoadObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.Loads.ActiveIndex = 0 then
        Exit;
        
    elem := TLoadObj(ActiveCircuit.Loads.Active);
    with elem do
    begin
        DailyShape := Value;
        LoadPropSideEffects(LoadProps.daily, elem);
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_duty(const Value: PAnsiChar); CDECL;
var
    elem: TLoadObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.Loads.ActiveIndex = 0 then
        Exit;
        
    elem := TLoadObj(ActiveCircuit.Loads.Active);
    with elem do
    begin
        DutyShape := Value;
        LoadPropSideEffects(LoadProps.duty, elem);
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Growth(const Value: PAnsiChar); CDECL;
var
    elem: TLoadObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.Loads.ActiveIndex = 0 then
        Exit;
        
    elem := TLoadObj(ActiveCircuit.Loads.Active);
    with elem do
    begin
        GrowthShape := Value;
        LoadPropSideEffects(LoadProps.growth, elem);
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_IsDelta(Value: Wordbool); CDECL;
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;
    if elem <> NIL then
        elem.Connection := Integer(Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kva(Value: Double); CDECL;
begin
    Set_Parameter('kva', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kwh(Value: Double); CDECL;
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;
    if elem = NIL then
        Exit;
    elem.Set_kWh(Value);
  //LoadPropSideEffects(LoadProps.kwh, elem);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kwhdays(Value: Double); CDECL;
begin
    Set_Parameter('kwhdays', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Model(Value: Integer); CDECL;
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;
    if elem <> NIL then
        elem.FLoadModel := Value; // enums match the integer codes
end;
//------------------------------------------------------------------------------
procedure Loads_Set_NumCust(Value: Integer); CDECL;
begin
    Set_Parameter('NumCust', IntToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PctMean(Value: Double); CDECL;
begin
    Set_Parameter('%mean', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PctStdDev(Value: Double); CDECL;
begin
    Set_Parameter('%stddev', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Rneut(Value: Double); CDECL;
begin
    Set_Parameter('Rneut', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Spectrum(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('Spectrum', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Status(Value: Integer); CDECL;
begin
    case Value of
        dssLoadVariable:
            Set_Parameter('status', 'v');
        dssLoadFixed:
            Set_Parameter('status', 'f');
        dssLoadExempt:
            Set_Parameter('status', 'e');
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vmaxpu(Value: Double); CDECL;
begin
    Set_Parameter('VmaxPu', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminemerg(Value: Double); CDECL;
begin
    Set_Parameter('VminEmerg', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminnorm(Value: Double); CDECL;
begin
    Set_Parameter('VminNorm', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminpu(Value: Double); CDECL;
begin
    Set_Parameter('VminPu', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_xfkVA(Value: Double); CDECL;
begin
    Set_Parameter('XfKVA', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Xneut(Value: Double); CDECL;
begin
    Set_Parameter('Xneut', FloatToStr(Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Yearly(const Value: PAnsiChar); CDECL;
var
    elem: TLoadObj;
begin
    if ActiveCircuit = nil then
        Exit;
    if ActiveCircuit.Loads.ActiveIndex = 0 then
        Exit;
        
    elem := TLoadObj(ActiveCircuit.Loads.Active);
    with elem do
    begin
        YearlyShape := Value;
        LoadPropSideEffects(LoadProps.yearly, elem);
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Get_ZIPV(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TLoadObj;
    k: Integer;
begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, 1);
    Result[0] := 0.0;  // error condition: one element array=0
    elem := ActiveLoad;
    if elem <> NIL then
    begin
        DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, elem.nZIPV);
        Move(elem.ZipV[1], ResultPtr[0], elem.nZIPV * SizeOf(Double));
    end;
end;

procedure Loads_Get_ZIPV_GR(); CDECL;
// Same as Loads_Get_ZIPV but uses global result (GR) pointers
begin
    Loads_Get_ZIPV(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;
//------------------------------------------------------------------------------
procedure Loads_Set_ZIPV(ValuePtr: PDouble; ValueCount: Integer); CDECL;
var
    Value: PDoubleArray;
    elem: TLoadObj;
    i, k: Integer;
begin
    if ValueCount <> 7 then
    begin
        DoSimpleMsg(Format('ZIPV requires 7 elements, %d were provided!', [ValueCount]), 5890);
        Exit;
    end;

    Value := PDoubleArray(ValuePtr);
    elem := ActiveLoad;
    if elem <> NIL then
    begin
        // allocate space for 7
        elem.nZIPV := 7;
        Move(ValuePtr[0], elem.ZIPV[1], elem.nZIPV * SizeOf(Double));
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_pctSeriesRL(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := -1.0; // signify  bad request
    elem := ActiveLoad;
    if elem <> NIL then
    begin
        Result := elem.puSeriesRL * 100.0;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_pctSeriesRL(Value: Double); CDECL;
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;

    if elem <> NIL then
    begin
        elem.puSeriesRL := Value / 100.0;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_RelWeight(): Double; CDECL;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.RelWeighting;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_RelWeight(Value: Double); CDECL;
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;
    if elem <> NIL then
        elem.RelWeighting := Value;
end;
//------------------------------------------------------------------------------
function Loads_Get_Phases(): Integer; CDECL;
var
    pLoad: TLoadObj;
begin
    Result := 0;
    pLoad := ActiveLoad;
    if pLoad <> NIL then
    begin
        Result := pLoad.Nphases;
    end
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Phases(Value: Integer); CDECL;
var
    pLoad: TLoadObj;
begin
    pLoad := ActiveLoad;
    if pLoad <> NIL then
    begin
        if (Value <> pLoad.NPhases) then
        begin
            pLoad.NPhases := Value;
            LoadPropSideEffects(LoadProps.phases, pLoad);
        end;
    end
end;
//------------------------------------------------------------------------------
function Loads_Get_Bus1_AnsiString(): Ansistring; inline;
var
    pLoad: TLoadObj;
begin
    pLoad := ActiveLoad;
    if pLoad = NIL then
        exit;
    Result := pLoad.GetBus(1);
end;

function Loads_Get_Bus1(): PAnsiChar; CDECL;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Bus1_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Bus1(const Value: PAnsiChar); CDECL;
var
    pLoad: TLoadObj;
begin
    pLoad := ActiveLoad;
    if pLoad = NIL then
        exit;
    pLoad.SetBus(1, Value);
    // LoadPropSideEffects(LoadProps.bus1, pLoad); -- Nothing
end;
//------------------------------------------------------------------------------
end.
