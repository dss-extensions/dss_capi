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
    // Some specials ...
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
        end;
    end;
end;
//------------------------------------------------------------------------------
function ActiveLoad: TLoadObj;
begin
    Result := NIL;
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Loads.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: String; const val: String);
var
    cmd: String;
begin
    if not Assigned(ActiveCircuit[ActiveActor]) then
        exit;
    SolutionAbort := FALSE;  // Reset for commands entered from outside
    cmd := Format('load.%s.%s=%s', [ActiveLoad.Name, parm, val]);
    DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
procedure Loads_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger); CDECL;
var
    Result: PPAnsiCharArray;
    LoadElem: TLoadObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    if ActiveCircuit[ActiveActor] <> NIL then
        with ActiveCircuit[ActiveActor] do
            if Loads.ListSize > 0 then
            begin
                DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Loads.ListSize - 1) + 1);
                k := 0;
                LoadElem := Loads.First;
                while LoadElem <> NIL do
                begin
                    Result[k] := DSS_CopyStringAsPChar(LoadElem.Name);
                    Inc(k);
                    LoadElem := Loads.Next;
                end;
            end;
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
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLoad := ActiveCircuit[ActiveActor].Loads.First;
        if pLoad <> NIL then
        begin
            repeat
                if pLoad.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                    Result := 1;
                end
                else
                    pLoad := ActiveCircuit[ActiveActor].Loads.Next;
            until (Result = 1) or (pLoad = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;
//------------------------------------------------------------------------------
function Loads_Get_idx(): Integer; CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
        Result := ActiveCircuit[ActiveActor].Loads.ActiveIndex
    else
        Result := 0;
end;
//------------------------------------------------------------------------------
function Loads_Get_Name_AnsiString(): Ansistring; inline;
var
    pLoad: TLoadObj;

begin
    Result := '';
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLoad := ActiveCircuit[ActiveActor].Loads.Active;
        if pLoad <> NIL then
            Result := pLoad.Name
        else
            Result := '';  // signify no name
    end;

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
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLoad := ActiveCircuit[ActiveActor].Loads.Next;
        if pLoad <> NIL then
        begin
            repeat
                if pLoad.Enabled then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                    Result := ActiveCircuit[ActiveActor].Loads.ActiveIndex;
                end
                else
                    pLoad := ActiveCircuit[ActiveActor].Loads.Next;
            until (Result > 0) or (pLoad = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;
//------------------------------------------------------------------------------
procedure Loads_Set_idx(Value: Integer); CDECL;
var
    pLoad: TLoadObj;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        pLoad := ActiveCircuit[ActiveActor].Loads.Get(Value);
        if pLoad <> NIL then
            ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Name(const Value: PAnsiChar); CDECL;
var
    ActiveSave: Integer;
    pLoad: TLoadObj;
    S: String;
    Found: Boolean;
begin

    if ActiveCircuit[ActiveActor] <> NIL then
    begin      // Search list of Loads in active circuit for name
        with ActiveCircuit[ActiveActor].Loads do
        begin
            S := Value;  // Convert to Pascal String
            Found := FALSE;
            ActiveSave := ActiveIndex;
            pLoad := First;
            while pLoad <> NIL do
            begin
                if (CompareText(pLoad.Name, S) = 0) then
                begin
                    ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
                    Found := TRUE;
                    Break;
                end;
                pLoad := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Load "' + S + '" Not Found in Active Circuit.', 5003);
                pLoad := Get(ActiveSave);    // Restore active Load
                ActiveCircuit[ActiveActor].ActiveCktElement := pLoad;
            end;
        end;
    end;

end;
//------------------------------------------------------------------------------
function Loads_Get_kV(): Double; CDECL;
begin
    Result := 0.0;
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
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
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
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
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
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
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
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
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
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
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                TLoadObj(Active).kvarBase := Value;
                TLoadObj(Active).LoadSpecType := 1;
                TLoadObj(Active).RecalcElementData(ActiveActor);  // set power factor based on kW, kvar
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kW(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                TLoadObj(Active).kWBase := Value;
                TLoadObj(Active).LoadSpecType := 0;
                TLoadObj(Active).RecalcElementData(ActiveActor); // sets kvar based on kW and pF
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PF(Value: Double); CDECL;
begin
    if ActiveCircuit[ActiveActor] <> NIL then
    begin
        with ActiveCircuit[ActiveActor].Loads do
        begin
            if ActiveIndex <> 0 then
            begin
                TLoadObj(Active).PFNominal := Value;
                TLoadObj(Active).LoadSpecType := 0;
                TLoadObj(Active).RecalcElementData(ActiveActor); //  sets kvar based on kW and pF
            end;
        end;
    end;
end;
//------------------------------------------------------------------------------
function Loads_Get_Count(): Integer; CDECL;
begin
    if Assigned(ActiveCircuit[ActiveActor]) then
        Result := ActiveCircuit[ActiveActor].Loads.ListSize;
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
begin
    Set_Parameter('Daily', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_duty(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('Duty', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Growth(const Value: PAnsiChar); CDECL;
begin
    Set_Parameter('Growth', Value);
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
begin
    Set_Parameter('kwh', FloatToStr(Value));
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
begin
    Set_Parameter('Yearly', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Get_ZIPV(var ResultPtr: PDouble; ResultCount: PInteger); CDECL;
var
    Result: PDoubleArray;
    elem: TLoadObj;
    k: Integer;

begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := 0.0;  // error condition: one element array=0
    elem := ActiveLoad;
    if elem <> NIL then
    begin
        DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (elem.nZIPV - 1) + 1);
        for k := 0 to elem.nZIPV - 1 do
            Result[k] := elem.ZipV^[k + 1];
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
        k := 1;
        for i := 0 to 6 do
        begin
            elem.ZIPV^[k] := Value[i];
            inc(k);
        end;
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

end.
