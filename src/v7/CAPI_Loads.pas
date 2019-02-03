UNIT CAPI_Loads;
{$inline on}

INTERFACE

USES CAPI_Utils;

PROCEDURE Loads_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
PROCEDURE Loads_Get_AllNames_GR();cdecl;
function Loads_Get_First():Integer;cdecl;
function Loads_Get_idx():Integer;cdecl;
function Loads_Get_Name():PAnsiChar;cdecl;
function Loads_Get_Next():Integer;cdecl;
procedure Loads_Set_idx(Value: Integer);cdecl;
procedure Loads_Set_Name(const Value: PAnsiChar);cdecl;
function Loads_Get_kV():Double;cdecl;
function Loads_Get_kvar():Double;cdecl;
function Loads_Get_kW():Double;cdecl;
function Loads_Get_PF():Double;cdecl;
procedure Loads_Set_kV(Value: Double);cdecl;
procedure Loads_Set_kvar(Value: Double);cdecl;
procedure Loads_Set_kW(Value: Double);cdecl;
procedure Loads_Set_PF(Value: Double);cdecl;
function Loads_Get_Count():Integer;cdecl;
function Loads_Get_AllocationFactor():Double;cdecl;
function Loads_Get_Cfactor():Double;cdecl;
function Loads_Get_Class_():Integer;cdecl;
function Loads_Get_CVRcurve():PAnsiChar;cdecl;
function Loads_Get_CVRvars():Double;cdecl;
function Loads_Get_CVRwatts():Double;cdecl;
function Loads_Get_daily():PAnsiChar;cdecl;
function Loads_Get_duty():PAnsiChar;cdecl;
function Loads_Get_Growth():PAnsiChar;cdecl;
function Loads_Get_IsDelta():WordBool;cdecl;
function Loads_Get_kva():Double;cdecl;
function Loads_Get_kwh():Double;cdecl;
function Loads_Get_kwhdays():Double;cdecl;
function Loads_Get_Model():Integer;cdecl;
function Loads_Get_NumCust():Integer;cdecl;
function Loads_Get_PctMean():Double;cdecl;
function Loads_Get_PctStdDev():Double;cdecl;
function Loads_Get_Rneut():Double;cdecl;
function Loads_Get_Spectrum():PAnsiChar;cdecl;
function Loads_Get_Status():Integer;cdecl;
function Loads_Get_Vmaxpu():Double;cdecl;
function Loads_Get_Vminemerg():Double;cdecl;
function Loads_Get_Vminnorm():Double;cdecl;
function Loads_Get_Vminpu():Double;cdecl;
function Loads_Get_xfkVA():Double;cdecl;
function Loads_Get_Xneut():Double;cdecl;
function Loads_Get_Yearly():PAnsiChar;cdecl;
procedure Loads_Set_AllocationFactor(Value: Double);cdecl;
procedure Loads_Set_Cfactor(Value: Double);cdecl;
procedure Loads_Set_Class_(Value: Integer);cdecl;
procedure Loads_Set_CVRcurve(const Value: PAnsiChar);cdecl;
procedure Loads_Set_CVRvars(Value: Double);cdecl;
procedure Loads_Set_CVRwatts(Value: Double);cdecl;
procedure Loads_Set_daily(const Value: PAnsiChar);cdecl;
procedure Loads_Set_duty(const Value: PAnsiChar);cdecl;
procedure Loads_Set_Growth(const Value: PAnsiChar);cdecl;
procedure Loads_Set_IsDelta(Value: WordBool);cdecl;
procedure Loads_Set_kva(Value: Double);cdecl;
procedure Loads_Set_kwh(Value: Double);cdecl;
procedure Loads_Set_kwhdays(Value: Double);cdecl;
procedure Loads_Set_Model(Value: Integer);cdecl;
procedure Loads_Set_NumCust(Value: Integer);cdecl;
procedure Loads_Set_PctMean(Value: Double);cdecl;
procedure Loads_Set_PctStdDev(Value: Double);cdecl;
procedure Loads_Set_Rneut(Value: Double);cdecl;
procedure Loads_Set_Spectrum(const Value: PAnsiChar);cdecl;
procedure Loads_Set_Status(Value: Integer);cdecl;
procedure Loads_Set_Vmaxpu(Value: Double);cdecl;
procedure Loads_Set_Vminemerg(Value: Double);cdecl;
procedure Loads_Set_Vminnorm(Value: Double);cdecl;
procedure Loads_Set_Vminpu(Value: Double);cdecl;
procedure Loads_Set_xfkVA(Value: Double);cdecl;
procedure Loads_Set_Xneut(Value: Double);cdecl;
procedure Loads_Set_Yearly(const Value: PAnsiChar);cdecl;
PROCEDURE Loads_Get_ZIPV(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
PROCEDURE Loads_Get_ZIPV_GR();cdecl;
procedure Loads_Set_ZIPV(ValuePtr: PDouble; ValueCount: Integer);cdecl;
function Loads_Get_pctSeriesRL():Double;cdecl;
procedure Loads_Set_pctSeriesRL(Value: Double);cdecl;
function Loads_Get_RelWeight():Double;cdecl;
procedure Loads_Set_RelWeight(Value: Double);cdecl;

// API extensions
FUNCTION Loads_Get_Phases():Integer;cdecl;
PROCEDURE Loads_Set_Phases(Value: Integer);cdecl;


IMPLEMENTATION

USES CAPI_Constants, DSSGlobals, Executive, Load, SysUtils, math;

TYPE
    LoadProps = (
        phases=1, bus1=2, kV=3, kW=4, pf=5, model=6, yearly=7, daily=8, duty=9, growth=10, conn=11, kvar=12, 
        Rneut=13, Xneut=14, status=15, cls=16, Vminpu=17, Vmaxpu=18, Vminnorm=19, Vminemerg=20, xfkVA=21, 
        allocationfactor=22, kVA=23, pctmean=24, pctstddev=25, CVRwatts=26, CVRvars=27, kwh=28, kwhdays=29, 
        Cfactor=30, CVRcurve=31, NumCust=32, ZIPV=33, pctSeriesRL=34, RelWeight=35, Vlowpu=36, 
        puXharm=37, XRhar=38
    );

//------------------------------------------------------------------------------
PROCEDURE LoadPropSideEffects(prop: LoadProps; load: TLoadObj); //incomplete
begin
  With load do
  begin
    // Some specials ...
    CASE prop OF
        LoadProps.phases:  Begin
            // -> SetNcondsForConnection  // Force Reallocation of terminal info
            CASE Connection OF
                0: NConds  := Fnphases +1;
                1: 
                    CASE Fnphases OF
                        1,2: NConds := Fnphases +1; // L-L and Open-delta
                    ELSE
                        NConds := Fnphases;
                    End;
            ELSE  {nada}
            End;
            // <- SetNcondsForConnection
            UpdateVoltageBases;
        End;
    END;
  end;
end;
//------------------------------------------------------------------------------
function ActiveLoad: TLoadObj;
begin
  Result := nil;
  if ActiveCircuit <> Nil then Result := ActiveCircuit.Loads.Active;
end;
//------------------------------------------------------------------------------
procedure Set_Parameter(const parm: string; const val: string);
var
  cmd: string;
begin
  if not Assigned (ActiveCircuit) then exit;
  SolutionAbort := FALSE;  // Reset for commands entered from outside
  cmd := Format ('load.%s.%s=%s', [ActiveLoad.Name, parm, val]);
  DSSExecutive.Command := cmd;
end;
//------------------------------------------------------------------------------
PROCEDURE Loads_Get_AllNames(var ResultPtr: PPAnsiChar; ResultCount: PInteger);cdecl;
VAR
  Result: PPAnsiCharArray;
  LoadElem:TLoadObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PPAnsiChar(ResultPtr, ResultCount, (0) + 1);
    Result[0] := DSS_CopyStringAsPChar('NONE');
    IF ActiveCircuit <> Nil THEN
     WITH ActiveCircuit DO
     If Loads.ListSize > 0 Then
     Begin
       DSS_RecreateArray_PPAnsiChar(Result, ResultPtr, ResultCount, (Loads.ListSize-1) + 1);
       k:=0;
       LoadElem := Loads.First;
       WHILE LoadElem<>Nil DO  Begin
          Result[k] := DSS_CopyStringAsPChar(LoadElem.Name);
          Inc(k);
          LoadElem := Loads.Next;
       End;
     End ;
end;
PROCEDURE Loads_Get_AllNames_GR();cdecl;
// Same as Loads_Get_AllNames but uses global result (GR) pointers
begin
   Loads_Get_AllNames(GR_DataPtr_PPAnsiChar, GR_CountPtr_PPAnsiChar)
end;

//------------------------------------------------------------------------------
function Loads_Get_First():Integer;cdecl;
Var
   pLoad:TLoadObj;

Begin

   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLoad := ActiveCircuit.Loads.First;
        If pLoad <> Nil Then
        Begin
          Repeat
            If pLoad.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLoad;
              Result := 1;
            End
            Else pLoad := ActiveCircuit.Loads.Next;
          Until (Result = 1) or (pLoad = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
function Loads_Get_idx():Integer;cdecl;
begin
    if ActiveCircuit <> Nil then
       Result := ActiveCircuit.Loads.ActiveIndex
    else Result := 0;
end;
//------------------------------------------------------------------------------
function Loads_Get_Name_AnsiString():AnsiString;inline;
Var
   pLoad:TLoadObj;

Begin
   Result := '';
   If ActiveCircuit <> Nil Then
   Begin
        pLoad := ActiveCircuit.Loads.Active;
        If pLoad <> Nil Then
          Result := pLoad.Name
        Else
            Result := '';  // signify no name
   End;

end;

function Loads_Get_Name():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Name_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_Next():Integer;cdecl;
Var
   pLoad:TLoadObj;

Begin
   Result := 0;
   If ActiveCircuit <> Nil Then
   Begin
        pLoad := ActiveCircuit.Loads.Next;
        If pLoad <> Nil Then
        Begin
          Repeat
            If pLoad.Enabled
            Then Begin
              ActiveCircuit.ActiveCktElement := pLoad;
              Result := ActiveCircuit.Loads.ActiveIndex;
            End
            Else pLoad := ActiveCircuit.Loads.Next;
          Until (Result > 0) or (pLoad = nil);
        End
        Else
            Result := 0;  // signify no more
   End;

end;
//------------------------------------------------------------------------------
procedure Loads_Set_idx(Value: Integer);cdecl;
Var
    pLoad:TLoadObj;
begin
    if ActiveCircuit <> Nil then   Begin
        pLoad := ActiveCircuit.Loads.Get(Value);
        If pLoad <> Nil Then ActiveCircuit.ActiveCktElement := pLoad;
    End;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Name(const Value: PAnsiChar);cdecl;
VAR
    ActiveSave :integer;
    pLoad:TLoadObj;
    S: String;
    Found :Boolean;
Begin

  IF ActiveCircuit <> NIL
  THEN Begin      // Search list of Loads in active circuit for name
     WITH ActiveCircuit.Loads DO
       Begin
           S := Value;  // Convert to Pascal String
           Found := FALSE;
           ActiveSave := ActiveIndex;
           pLoad := First;
           While pLoad <> NIL Do
           Begin
              IF (CompareText(pLoad.Name, S) = 0)
              THEN Begin
                  ActiveCircuit.ActiveCktElement := pLoad;
                  Found := TRUE;
                  Break;
              End;
              pLoad := Next;
           End;
           IF NOT Found
           THEN Begin
               DoSimpleMsg('Load "' + S + '" Not Found in Active Circuit.', 5003);
               pLoad := Get(ActiveSave);    // Restore active Load
               ActiveCircuit.ActiveCktElement := pLoad;
           End;
       End;
  End;

end;
//------------------------------------------------------------------------------
function Loads_Get_kV():Double;cdecl;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kVLoadBase;
             End;
         End;
   End;

end;
//------------------------------------------------------------------------------
function Loads_Get_kvar():Double;cdecl;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kvarBase;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Loads_Get_kW():Double;cdecl;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).kWBase;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Loads_Get_PF():Double;cdecl;
begin
   Result := 0.0;
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                 Result := TLoadObj(Active).PFNominal;
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kV(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kVLoadBase := Value;
                  TLoadObj(Active).UpdateVoltageBases;  // side effects
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kvar(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kvarBase := Value;
                  TLoadObj(Active).LoadSpecType := 1;
                  TLoadObj(Active).RecalcElementData ;  // set power factor based on kW, kvar
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kW(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).kWBase := Value;
                  TLoadObj(Active).LoadSpecType := 0;
                  TLoadObj(Active).RecalcElementData ; // sets kvar based on kW and pF
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PF(Value: Double);cdecl;
begin
   IF ActiveCircuit<> NIL THEN Begin
         WITH ActiveCircuit.Loads Do Begin
             IF ActiveIndex<>0 THEN Begin
                  TLoadObj(Active).PFNominal := Value;
                  TLoadObj(Active).LoadSpecType := 0;
                  TLoadObj(Active).RecalcElementData ; //  sets kvar based on kW and pF
             End;
         End;
   End;
end;
//------------------------------------------------------------------------------
function Loads_Get_Count():Integer;cdecl;
begin
    If Assigned(ActiveCircuit) Then
       Result := ActiveCircuit.Loads.ListSize ;
end;
//------------------------------------------------------------------------------
function Loads_Get_AllocationFactor():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.AllocationFactor;
end;
//------------------------------------------------------------------------------
function Loads_Get_Cfactor():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.CFactor;
end;
//------------------------------------------------------------------------------
function Loads_Get_Class_():Integer;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.LoadClass;
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRcurve_AnsiString():AnsiString;inline;
var
  elem: TLoadObj;
begin
  Result := '';
  elem := ActiveLoad;
  if elem <> nil then Result := elem.CVRshape;
end;

function Loads_Get_CVRcurve():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_CVRcurve_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRvars():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.CVRvars;
end;
//------------------------------------------------------------------------------
function Loads_Get_CVRwatts():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.CVRwatts;
end;
//------------------------------------------------------------------------------
function Loads_Get_daily_AnsiString():AnsiString;inline;
var
  elem: TLoadObj;
begin
  Result := '';
  elem := ActiveLoad;
  if elem <> nil then Result := elem.DailyShape;
end;

function Loads_Get_daily():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_daily_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_duty_AnsiString():AnsiString;inline;
var
  elem: TLoadObj;
begin
  Result := '';
  elem := ActiveLoad;
  if elem <> nil then Result := elem.DailyShape;
end;

function Loads_Get_duty():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_duty_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_Growth_AnsiString():AnsiString;inline;
var
  elem: TLoadObj;
begin
  Result := '';
  elem := ActiveLoad;
  if elem <> nil then Result := elem.GrowthShape;
end;

function Loads_Get_Growth():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Growth_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_IsDelta():WordBool;cdecl;
var
  elem: TLoadObj;
begin
  Result := FALSE;
  elem := ActiveLoad;
  if elem <> nil then if elem.Connection > 0 then Result := TRUE;
end;
//------------------------------------------------------------------------------
function Loads_Get_kva():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.kVABase;
end;
//------------------------------------------------------------------------------
function Loads_Get_kwh():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.kWh;
end;
//------------------------------------------------------------------------------
function Loads_Get_kwhdays():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.kWhDays;
end;
//------------------------------------------------------------------------------
function Loads_Get_Model():Integer;cdecl;
var
  elem: TLoadObj;
begin
  Result := dssLoadConstPQ;
  elem := ActiveLoad;
  if elem <> nil then begin
    case elem.FLoadModel of
      1: Result := dssLoadConstPQ;
      2: Result := dssLoadConstZ;
      3: Result := dssLoadMotor;
      4: Result := dssLoadCVR;
      5: Result := dssLoadConstI;
      6: Result := dssLoadConstPFixedQ;
      7: Result := dssLoadConstPFixedX;
      8: Result := dssLoadZIPV;
    end;
  end;
end;
//------------------------------------------------------------------------------
function Loads_Get_NumCust():Integer;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.NumCustomers;
end;
//------------------------------------------------------------------------------
function Loads_Get_PctMean():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.puMean * 100.0;
end;
//------------------------------------------------------------------------------
function Loads_Get_PctStdDev():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.puStdDev * 100.0;
end;
//------------------------------------------------------------------------------
function Loads_Get_Rneut():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.Rneut;
end;
//------------------------------------------------------------------------------
function Loads_Get_Spectrum_AnsiString():AnsiString;inline;
var
  elem: TLoadObj;
begin
  Result := '';
  elem := ActiveLoad;
  if elem <> nil then Result := elem.Spectrum;
end;

function Loads_Get_Spectrum():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Spectrum_AnsiString());
end;
//------------------------------------------------------------------------------
function Loads_Get_Status():Integer;cdecl;
var
  elem: TLoadObj;
begin
  Result := dssLoadVariable;
  elem := ActiveLoad;
  if elem <> nil then begin
    if elem.ExemptLoad then
      Result := dssLoadExempt
    else if elem.FixedLoad then
      Result := dssLoadFixed;
  end;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vmaxpu():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.MaxPU;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminemerg():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.MinEmerg;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminnorm():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.MinNormal;
end;
//------------------------------------------------------------------------------
function Loads_Get_Vminpu():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.MinPU;
end;
//------------------------------------------------------------------------------
function Loads_Get_xfkVA():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.ConnectedkVA;
end;
//------------------------------------------------------------------------------
function Loads_Get_Xneut():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.Xneut;
end;
//------------------------------------------------------------------------------
function Loads_Get_Yearly_AnsiString():AnsiString;inline;
var
  elem: TLoadObj;
begin
  Result := '';
  elem := ActiveLoad;
  if elem <> nil then Result := elem.YearlyShape;
end;

function Loads_Get_Yearly():PAnsiChar;cdecl;
begin
    Result := DSS_GetAsPAnsiChar(Loads_Get_Yearly_AnsiString());
end;
//------------------------------------------------------------------------------
procedure Loads_Set_AllocationFactor(Value: Double);cdecl;
begin
  Set_Parameter ('AllocationFactor', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Cfactor(Value: Double);cdecl;
begin
  Set_Parameter ('Cfactor', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Class_(Value: Integer);cdecl;
begin
  Set_Parameter ('Class', IntToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRcurve(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('CVRcurve', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRvars(Value: Double);cdecl;
begin
  Set_Parameter ('CVRvars', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_CVRwatts(Value: Double);cdecl;
begin
  Set_Parameter ('CVRwatts', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_daily(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Daily', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_duty(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Duty', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Growth(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Growth', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_IsDelta(Value: WordBool);cdecl;
var
  elem: TLoadObj;
begin
  elem := ActiveLoad;
  if elem <> nil then elem.Connection := Integer (Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kva(Value: Double);cdecl;
begin
  Set_Parameter ('kva', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kwh(Value: Double);cdecl;
begin
  Set_Parameter ('kwh', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_kwhdays(Value: Double);cdecl;
begin
  Set_Parameter ('kwhdays', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Model(Value: Integer);cdecl;
var
  elem: TLoadObj;
begin
  elem := ActiveLoad;
  if elem <> nil then elem.FLoadModel := Value; // enums match the integer codes
end;
//------------------------------------------------------------------------------
procedure Loads_Set_NumCust(Value: Integer);cdecl;
begin
  Set_Parameter ('NumCust', IntToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PctMean(Value: Double);cdecl;
begin
  Set_Parameter ('%mean', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_PctStdDev(Value: Double);cdecl;
begin
  Set_Parameter ('%stddev', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Rneut(Value: Double);cdecl;
begin
  Set_Parameter ('Rneut', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Spectrum(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Spectrum', Value);
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Status(Value: Integer);cdecl;
begin
  case Value of
    dssLoadVariable: Set_Parameter('status', 'v');
    dssLoadFixed: Set_Parameter('status', 'f');
    dssLoadExempt: Set_Parameter('status', 'e');
  end;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vmaxpu(Value: Double);cdecl;
begin
  Set_Parameter ('VmaxPu', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminemerg(Value: Double);cdecl;
begin
  Set_Parameter ('VminEmerg', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminnorm(Value: Double);cdecl;
begin
  Set_Parameter ('VminNorm', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Vminpu(Value: Double);cdecl;
begin
  Set_Parameter ('VminPu', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_xfkVA(Value: Double);cdecl;
begin
  Set_Parameter ('XfKVA', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Xneut(Value: Double);cdecl;
begin
  Set_Parameter ('Xneut', FloatToStr (Value));
end;
//------------------------------------------------------------------------------
procedure Loads_Set_Yearly(const Value: PAnsiChar);cdecl;
begin
  Set_Parameter ('Yearly', Value);
end;
//------------------------------------------------------------------------------
PROCEDURE Loads_Get_ZIPV(var ResultPtr: PDouble; ResultCount: PInteger);cdecl;
VAR
  Result: PDoubleArray;
  elem:TLoadObj;
  k:Integer;

Begin
    Result := DSS_RecreateArray_PDouble(ResultPtr, ResultCount, (0) + 1);
    Result[0] := 0.0;  // error condition: one element array=0
    elem := ActiveLoad;
    IF elem <> Nil THEN
    Begin
         DSS_RecreateArray_PDouble(Result, ResultPtr, ResultCount, (elem.nZIPV-1) + 1);
         For k:=0 to elem.nZIPV-1 Do
              Result[k] := elem.ZipV^[k+1];
    End ;

end;
PROCEDURE Loads_Get_ZIPV_GR();cdecl;
// Same as Loads_Get_ZIPV but uses global result (GR) pointers
begin
   Loads_Get_ZIPV(GR_DataPtr_PDouble, GR_CountPtr_PDouble)
end;

//------------------------------------------------------------------------------
procedure Loads_Set_ZIPV(ValuePtr: PDouble; ValueCount: Integer);cdecl;
VAR
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
    If elem <> nil Then
    Begin
         // allocate space for 7
         elem.nZIPV := 7;
         k := 1;
         for i := 0 to 6 do
         Begin
             elem.ZIPV^[k] := Value[i];
             inc(k);
         End;
    End;
end;
//------------------------------------------------------------------------------
function Loads_Get_pctSeriesRL():Double;cdecl;
Var
  elem:TLoadObj;

begin
       Result := -1.0; // signify  bad request
       elem := ActiveLoad;
       If elem <> nil Then
       Begin
           Result := elem.puSeriesRL * 100.0;
       End;
end;
//------------------------------------------------------------------------------
procedure Loads_Set_pctSeriesRL(Value: Double);cdecl;
Var
  elem:TLoadObj;

begin
       elem := ActiveLoad;

       If elem <> nil Then
       Begin
            elem.puSeriesRL  := Value / 100.0;
       End;
end;
//------------------------------------------------------------------------------
function Loads_Get_RelWeight():Double;cdecl;
var
  elem: TLoadObj;
begin
  Result := 0.0;
  elem := ActiveLoad;
  if elem <> nil then Result := elem.RelWeighting;

end;
//------------------------------------------------------------------------------
procedure Loads_Set_RelWeight(Value: Double);cdecl;
var
  elem: TLoadObj;
begin
  elem := ActiveLoad;
  if elem <> nil then elem.RelWeighting := Value;
end;
//------------------------------------------------------------------------------
function Loads_Get_Phases():Integer;cdecl;
var
    pLoad: TLoadObj;
begin
    Result := 0;
    pLoad := ActiveLoad;
    if pLoad <> Nil
    THEN Begin
        Result := pLoad.Nphases;
    End
end;
//------------------------------------------------------------------------------
PROCEDURE Loads_Set_Phases(Value: Integer);cdecl;
var
    pLoad: TLoadObj;
begin
    pLoad := ActiveLoad;
    if pLoad <> Nil
    THEN Begin
        if (Value <> pLoad.NPhases) then
        begin
            pLoad.NPhases := Value;
            LoadPropSideEffects(LoadProps.phases, pLoad);
        end;
    End
end;
//------------------------------------------------------------------------------

END.
