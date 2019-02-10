unit ImplLoads;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}
{$WARN SYMBOL_PLATFORM OFF}

interface

uses
    ComObj,
    ActiveX,
    OpenDSSengine_TLB,
    StdVcl;

type
    TLoads = class(TAutoObject, ILoads)
    PROTECTED
        function Get_AllNames: Olevariant; SAFECALL;
        function Get_First: Integer; SAFECALL;
        function Get_idx: Integer; SAFECALL;
        function Get_Name: Widestring; SAFECALL;
        function Get_Next: Integer; SAFECALL;
        procedure Set_idx(Value: Integer); SAFECALL;
        procedure Set_Name(const Value: Widestring); SAFECALL;
        function Get_kV: Double; SAFECALL;
        function Get_kvar: Double; SAFECALL;
        function Get_kW: Double; SAFECALL;
        function Get_PF: Double; SAFECALL;
        procedure Set_kV(Value: Double); SAFECALL;
        procedure Set_kvar(Value: Double); SAFECALL;
        procedure Set_kW(Value: Double); SAFECALL;
        procedure Set_PF(Value: Double); SAFECALL;
        function Get_Count: Integer; SAFECALL;
        function Get_AllocationFactor: Double; SAFECALL;
        function Get_Cfactor: Double; SAFECALL;
        function Get_Class_: Integer; SAFECALL;
        function Get_CVRcurve: Widestring; SAFECALL;
        function Get_CVRvars: Double; SAFECALL;
        function Get_CVRwatts: Double; SAFECALL;
        function Get_daily: Widestring; SAFECALL;
        function Get_duty: Widestring; SAFECALL;
        function Get_Growth: Widestring; SAFECALL;
        function Get_IsDelta: Wordbool; SAFECALL;
        function Get_kva: Double; SAFECALL;
        function Get_kwh: Double; SAFECALL;
        function Get_kwhdays: Double; SAFECALL;
        function Get_Model: LoadModels; SAFECALL;
        function Get_NumCust: Integer; SAFECALL;
        function Get_PctMean: Double; SAFECALL;
        function Get_PctStdDev: Double; SAFECALL;
        function Get_Rneut: Double; SAFECALL;
        function Get_Spectrum: Widestring; SAFECALL;
        function Get_Status: LoadStatus; SAFECALL;
        function Get_Vmaxpu: Double; SAFECALL;
        function Get_Vminemerg: Double; SAFECALL;
        function Get_Vminnorm: Double; SAFECALL;
        function Get_Vminpu: Double; SAFECALL;
        function Get_xfkVA: Double; SAFECALL;
        function Get_Xneut: Double; SAFECALL;
        function Get_Yearly: Widestring; SAFECALL;
        procedure Set_AllocationFactor(Value: Double); SAFECALL;
        procedure Set_Cfactor(Value: Double); SAFECALL;
        procedure Set_Class_(Value: Integer); SAFECALL;
        procedure Set_CVRcurve(const Value: Widestring); SAFECALL;
        procedure Set_CVRvars(Value: Double); SAFECALL;
        procedure Set_CVRwatts(Value: Double); SAFECALL;
        procedure Set_daily(const Value: Widestring); SAFECALL;
        procedure Set_duty(const Value: Widestring); SAFECALL;
        procedure Set_Growth(const Value: Widestring); SAFECALL;
        procedure Set_IsDelta(Value: Wordbool); SAFECALL;
        procedure Set_kva(Value: Double); SAFECALL;
        procedure Set_kwh(Value: Double); SAFECALL;
        procedure Set_kwhdays(Value: Double); SAFECALL;
        procedure Set_Model(Value: LoadModels); SAFECALL;
        procedure Set_NumCust(Value: Integer); SAFECALL;
        procedure Set_PctMean(Value: Double); SAFECALL;
        procedure Set_PctStdDev(Value: Double); SAFECALL;
        procedure Set_Rneut(Value: Double); SAFECALL;
        procedure Set_Spectrum(const Value: Widestring); SAFECALL;
        procedure Set_Status(Value: LoadStatus); SAFECALL;
        procedure Set_Vmaxpu(Value: Double); SAFECALL;
        procedure Set_Vminemerg(Value: Double); SAFECALL;
        procedure Set_Vminnorm(Value: Double); SAFECALL;
        procedure Set_Vminpu(Value: Double); SAFECALL;
        procedure Set_xfkVA(Value: Double); SAFECALL;
        procedure Set_Xneut(Value: Double); SAFECALL;
        procedure Set_Yearly(const Value: Widestring); SAFECALL;
        function Get_ZIPV: Olevariant; SAFECALL;
        procedure Set_ZIPV(Value: Olevariant); SAFECALL;
        function Get_pctSeriesRL: Double; SAFECALL;
        procedure Set_pctSeriesRL(Value: Double); SAFECALL;
        function Get_RelWeight: Double; SAFECALL;
        procedure Set_RelWeight(Value: Double); STDCALL;

    end;

implementation

uses
    ComServ,
    DSSGlobals,
    Executive,
    Load,
    Variants,
    SysUtils,
    math;

function ActiveLoad: TLoadObj;
begin
    Result := NIL;
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Loads.Active;
end;

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

function TLoads.Get_AllNames: Olevariant;
var
    LoadElem: TLoadObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varOleStr);
    Result[0] := 'NONE';
    if ActiveCircuit <> NIL then
        with ActiveCircuit do
            if Loads.ListSize > 0 then
            begin
                VarArrayRedim(Result, Loads.ListSize - 1);
                k := 0;
                LoadElem := Loads.First;
                while LoadElem <> NIL do
                begin
                    Result[k] := LoadElem.Name;
                    Inc(k);
                    LoadElem := Loads.Next;
                end;
            end;
end;

function TLoads.Get_First: Integer;
var
    pLoad: TLoadObj;

begin

    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLoad := ActiveCircuit.Loads.First;
        if pLoad <> NIL then
        begin
            repeat
                if pLoad.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pLoad;
                    Result := 1;
                end
                else
                    pLoad := ActiveCircuit.Loads.Next;
            until (Result = 1) or (pLoad = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;

function TLoads.Get_idx: Integer;
begin
    if ActiveCircuit <> NIL then
        Result := ActiveCircuit.Loads.ActiveIndex
    else
        Result := 0;
end;

function TLoads.Get_Name: Widestring;
var
    pLoad: TLoadObj;

begin
    Result := '';
    if ActiveCircuit <> NIL then
    begin
        pLoad := ActiveCircuit.Loads.Active;
        if pLoad <> NIL then
            Result := pLoad.Name
        else
            Result := '';  // signify no name
    end;

end;

function TLoads.Get_Next: Integer;
var
    pLoad: TLoadObj;

begin
    Result := 0;
    if ActiveCircuit <> NIL then
    begin
        pLoad := ActiveCircuit.Loads.Next;
        if pLoad <> NIL then
        begin
            repeat
                if pLoad.Enabled then
                begin
                    ActiveCircuit.ActiveCktElement := pLoad;
                    Result := ActiveCircuit.Loads.ActiveIndex;
                end
                else
                    pLoad := ActiveCircuit.Loads.Next;
            until (Result > 0) or (pLoad = NIL);
        end
        else
            Result := 0;  // signify no more
    end;

end;

procedure TLoads.Set_idx(Value: Integer);
var
    pLoad: TLoadObj;
begin
    if ActiveCircuit <> NIL then
    begin
        pLoad := ActiveCircuit.Loads.Get(Value);
        if pLoad <> NIL then
            ActiveCircuit.ActiveCktElement := pLoad;
    end;
end;

procedure TLoads.Set_Name(const Value: Widestring);
var
    ActiveSave: Integer;
    pLoad: TLoadObj;
    S: String;
    Found: Boolean;
begin

    if ActiveCircuit <> NIL then
    begin      // Search list of Loads in active circuit for name
        with ActiveCircuit.Loads do
        begin
            S := Value;  // Convert to Pascal String
            Found := FALSE;
            ActiveSave := ActiveIndex;
            pLoad := First;
            while pLoad <> NIL do
            begin
                if (CompareText(pLoad.Name, S) = 0) then
                begin
                    ActiveCircuit.ActiveCktElement := pLoad;
                    Found := TRUE;
                    Break;
                end;
                pLoad := Next;
            end;
            if not Found then
            begin
                DoSimpleMsg('Load "' + S + '" Not Found in Active Circuit.', 5003);
                pLoad := Get(ActiveSave);    // Restore active Load
                ActiveCircuit.ActiveCktElement := pLoad;
            end;
        end;
    end;

end;

function TLoads.Get_kV: Double;
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

function TLoads.Get_kvar: Double;
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

function TLoads.Get_kW: Double;
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

function TLoads.Get_PF: Double;
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

procedure TLoads.Set_kV(Value: Double);
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

procedure TLoads.Set_kvar(Value: Double);
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

procedure TLoads.Set_kW(Value: Double);
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

procedure TLoads.Set_PF(Value: Double);
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

function TLoads.Get_Count: Integer;
begin
    if Assigned(ActiveCircuit) then
        Result := ActiveCircuit.Loads.ListSize;
end;

function TLoads.Get_AllocationFactor: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.AllocationFactor;
end;

function TLoads.Get_Cfactor: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CFactor;
end;

function TLoads.Get_Class_: Integer;
var
    elem: TLoadObj;
begin
    Result := 0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.LoadClass;
end;

function TLoads.Get_CVRcurve: Widestring;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CVRshape;
end;

function TLoads.Get_CVRvars: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CVRvars;
end;

function TLoads.Get_CVRwatts: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.CVRwatts;
end;

function TLoads.Get_daily: Widestring;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.DailyShape;
end;

function TLoads.Get_duty: Widestring;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.DailyShape;
end;

function TLoads.Get_Growth: Widestring;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.GrowthShape;
end;

function TLoads.Get_IsDelta: Wordbool;
var
    elem: TLoadObj;
begin
    Result := FALSE;
    elem := ActiveLoad;
    if elem <> NIL then
        if elem.Connection > 0 then
            Result := TRUE;
end;

function TLoads.Get_kva: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.kVABase;
end;

function TLoads.Get_kwh: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.kWh;
end;

function TLoads.Get_kwhdays: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.kWhDays;
end;

function TLoads.Get_Model: LoadModels;
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

function TLoads.Get_NumCust: Integer;
var
    elem: TLoadObj;
begin
    Result := 0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.NumCustomers;
end;

function TLoads.Get_PctMean: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.puMean * 100.0;
end;

function TLoads.Get_PctStdDev: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.puStdDev * 100.0;
end;

function TLoads.Get_Rneut: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.Rneut;
end;

function TLoads.Get_Spectrum: Widestring;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.Spectrum;
end;

function TLoads.Get_Status: LoadStatus;
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

function TLoads.Get_Vmaxpu: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MaxPU;
end;

function TLoads.Get_Vminemerg: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MinEmerg;
end;

function TLoads.Get_Vminnorm: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MinNormal;
end;

function TLoads.Get_Vminpu: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.MinPU;
end;

function TLoads.Get_xfkVA: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.ConnectedkVA;
end;

function TLoads.Get_Xneut: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.Xneut;
end;

function TLoads.Get_Yearly: Widestring;
var
    elem: TLoadObj;
begin
    Result := '';
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.YearlyShape;
end;

procedure TLoads.Set_AllocationFactor(Value: Double);
begin
    Set_Parameter('AllocationFactor', FloatToStr(Value));
end;

procedure TLoads.Set_Cfactor(Value: Double);
begin
    Set_Parameter('Cfactor', FloatToStr(Value));
end;

procedure TLoads.Set_Class_(Value: Integer);
begin
    Set_Parameter('Class', IntToStr(Value));
end;

procedure TLoads.Set_CVRcurve(const Value: Widestring);
begin
    Set_Parameter('CVRcurve', Value);
end;

procedure TLoads.Set_CVRvars(Value: Double);
begin
    Set_Parameter('CVRvars', FloatToStr(Value));
end;

procedure TLoads.Set_CVRwatts(Value: Double);
begin
    Set_Parameter('CVRwatts', FloatToStr(Value));
end;

procedure TLoads.Set_daily(const Value: Widestring);
begin
    Set_Parameter('Daily', Value);
end;

procedure TLoads.Set_duty(const Value: Widestring);
begin
    Set_Parameter('Duty', Value);
end;

procedure TLoads.Set_Growth(const Value: Widestring);
begin
    Set_Parameter('Growth', Value);
end;

procedure TLoads.Set_IsDelta(Value: Wordbool);
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;
    if elem <> NIL then
        elem.Connection := Integer(Value);
end;

procedure TLoads.Set_kva(Value: Double);
begin
    Set_Parameter('kva', FloatToStr(Value));
end;

procedure TLoads.Set_kwh(Value: Double);
begin
    Set_Parameter('kwh', FloatToStr(Value));
end;

procedure TLoads.Set_kwhdays(Value: Double);
begin
    Set_Parameter('kwhdays', FloatToStr(Value));
end;

procedure TLoads.Set_Model(Value: LoadModels);
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;
    if elem <> NIL then
        elem.FLoadModel := Value; // enums match the integer codes
end;

procedure TLoads.Set_NumCust(Value: Integer);
begin
    Set_Parameter('NumCust', IntToStr(Value));
end;

procedure TLoads.Set_PctMean(Value: Double);
begin
    Set_Parameter('%mean', FloatToStr(Value));
end;

procedure TLoads.Set_PctStdDev(Value: Double);
begin
    Set_Parameter('%stddev', FloatToStr(Value));
end;

procedure TLoads.Set_Rneut(Value: Double);
begin
    Set_Parameter('Rneut', FloatToStr(Value));
end;

procedure TLoads.Set_Spectrum(const Value: Widestring);
begin
    Set_Parameter('Spectrum', Value);
end;

procedure TLoads.Set_Status(Value: LoadStatus);
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

procedure TLoads.Set_Vmaxpu(Value: Double);
begin
    Set_Parameter('VmaxPu', FloatToStr(Value));
end;

procedure TLoads.Set_Vminemerg(Value: Double);
begin
    Set_Parameter('VminEmerg', FloatToStr(Value));
end;

procedure TLoads.Set_Vminnorm(Value: Double);
begin
    Set_Parameter('VminNorm', FloatToStr(Value));
end;

procedure TLoads.Set_Vminpu(Value: Double);
begin
    Set_Parameter('VminPu', FloatToStr(Value));
end;

procedure TLoads.Set_xfkVA(Value: Double);
begin
    Set_Parameter('XfKVA', FloatToStr(Value));
end;

procedure TLoads.Set_Xneut(Value: Double);
begin
    Set_Parameter('Xneut', FloatToStr(Value));
end;

procedure TLoads.Set_Yearly(const Value: Widestring);
begin
    Set_Parameter('Yearly', Value);
end;

function TLoads.Get_ZIPV: Olevariant;
var
    elem: TLoadObj;
    k: Integer;

begin
    Result := VarArrayCreate([0, 0], varDouble);
    Result[0] := 0.0;  // error condition: one element array=0
    elem := ActiveLoad;
    if elem <> NIL then
    begin
        VarArrayRedim(Result, elem.nZIPV - 1);
        for k := 0 to elem.nZIPV - 1 do
            Result[k] := elem.ZipV^[k + 1];
    end;

end;

procedure TLoads.Set_ZIPV(Value: Olevariant);

var
    elem: TLoadObj;
    i, k, LoopLimit: Integer;

begin
    elem := ActiveLoad;
    if elem <> NIL then
    begin
         // allocate space for 7
        elem.nZIPV := 7;
         // only put as many elements as proviced up to nZIPV
        LoopLimit := VarArrayHighBound(Value, 1);
        if (LoopLimit - VarArrayLowBound(Value, 1) + 1) > 7 then
            LoopLimit := VarArrayLowBound(Value, 1) + 6;

        k := 1;
        for i := VarArrayLowBound(Value, 1) to LoopLimit do
        begin
            elem.ZIPV^[k] := Value[i];
            inc(k);
        end;
    end;
end;

function TLoads.Get_pctSeriesRL: Double;

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

procedure TLoads.Set_pctSeriesRL(Value: Double);
var
    elem: TLoadObj;

begin
    elem := ActiveLoad;

    if elem <> NIL then
    begin
        elem.puSeriesRL := Value / 100.0;
    end;
end;


function TLoads.Get_RelWeight: Double;
var
    elem: TLoadObj;
begin
    Result := 0.0;
    elem := ActiveLoad;
    if elem <> NIL then
        Result := elem.RelWeighting;

end;

procedure TLoads.Set_RelWeight(Value: Double);
var
    elem: TLoadObj;
begin
    elem := ActiveLoad;
    if elem <> NIL then
        elem.RelWeighting := Value;
end;

initialization
    TAutoObjectFactory.Create(ComServer, TLoads, Class_Loads,
        ciInternal, tmApartment);
end.
