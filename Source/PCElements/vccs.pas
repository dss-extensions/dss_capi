unit VCCS;
{
  ----------------------------------------------------------
  Copyright (c) 2016, University of Pittsburgh
  Copyright (c) 2019-2021, Battelle Memorial Institute
  All rights reserved.
  ----------------------------------------------------------
}

{$ifdef fpc}{$mode delphi}{$endif}

interface

USES DSSClass, PCClass,PCElement, ucmatrix, ucomplex, XYCurve, ArrayDef;

TYPE
   TVCCS = CLASS(TPCClass)
     private
       XY_CurveClass: TDSSClass;
     Protected
       Procedure DefineProperties;
       Function MakeLike(Const OtherSource:STring):Integer;Override;
     public
       constructor Create;
       destructor Destroy; override;

       Function Edit:Integer; override;
       Function Init(Handle:Integer):Integer; override;
       Function NewObject(const ObjName:String):Integer; override;
   End;

   TVCCSObj = class(TPCElement)
     private
        Fbp1: TXYcurveObj;
        Fbp1_name: String;
        Fbp2: TXYcurveObj;
        Fbp2_name: String;
        Ffilter: TXYcurveObj;
        Ffilter_name: String;
        BaseCurr: double; // line current at Ppct
        BaseVolt: double; // line-to-neutral voltage at Vrated
        FsampleFreq: double; // discretization frequency for Z filter
        Fwinlen: integer;
        Ffiltlen: integer;
        Irated: double; // line current at full output
        Fkv: double; // scale voltage to HW pu input
        Fki: double; // scale HW pu output to current

        FrmsMode: Boolean; // indicates a phasor-domain PLL simulation
        FmaxIpu: double; // maximum RMS current in per-unit of rated
        FvrmsTau: double; // LPF time constant sensing Vrms
        FirmsTau: double; // LPF time constant producing Irms

        // Support for Dynamics Mode - PU of BaseVolt and BaseCurr
        // state variables for Dynamics Mode
        s1: double; // Vwave(t), or Vrms in phasor mode
        s2: double; // Iwave(t), or Ipwr in phasor mode
        s3: double; // Irms,     or Hout in phasor mode
        s4: double; // Ipeak,    or Irms in phasor mode
        s5: double; // BP1out,   or NA in phasor mode
        s6: double; // Hout,     or NA in phasor mode
        sV1: complex; // positive-sequence voltage; use to inject I1 only

        vlast: complex;
        y2: pDoubleArray;
        z: pDoubleArray;     // current digital filter history terms
        whist: pDoubleArray;
        zlast: pDoubleArray; // update only after the corrector step
        wlast: pDoubleArray;
        sIdxU: integer; // ring buffer index for z and whist
        sIdxY: integer; // ring buffer index for y2 (rms current)
        y2sum: double;
        procedure InitPhasorStates;
        procedure IntegratePhasorStates;
        procedure ShutoffInjections;
        procedure UpdateSequenceVoltage;

     protected
        Function  Get_Variable(i: Integer): Double; Override;
        procedure Set_Variable(i: Integer; Value: Double); Override;
      public
        Ppct, Prated, Vrated: double;
        constructor Create(ParClass:TDSSClass; const SourceName:String);
        destructor  Destroy; override;

        Procedure RecalcElementData; Override;
        Procedure CalcYPrim; Override;

        PROCEDURE MakePosSequence;Override;  // Make a positive Sequence Model

        Function  InjCurrents:Integer; Override;
        Procedure GetInjCurrents(Curr:pComplexArray); Override;
        Procedure GetCurrents(Curr: pComplexArray);Override;

        PROCEDURE InitPropertyValues(ArrayOffset:Integer);Override;
        Procedure DumpProperties(Var F:TextFile; Complete:Boolean); Override;

        // Support for Dynamics Mode
        Procedure InitStateVars; Override;
        Procedure IntegrateStates; Override;
        Function NumVariables:Integer; Override;
        Procedure GetAllVariables(States:pDoubleArray); Override;
        Function VariableName(i:Integer):String; Override;
   End;

VAR
    ActiveVCCSObj:TVCCSObj;
    VCCSClass:TVCCS;

implementation

USES  ParserDel, Circuit, DSSClassDefs, DSSGlobals, Utilities, Sysutils, Command,
      Solution;

Var  NumPropsThisClass:Integer;
     ALPHA1, ALPHA2: complex;

// helper functions for ring buffer indexing, 1..len
function MapIdx(idx, len: integer):integer;
begin
  while idx <= 0 do idx := idx + len;
  Result := idx mod (len + 1);
  if Result = 0 then Result := 1;
end;

function OffsetIdx(idx, offset, len: integer):integer;
begin
  Result := MapIdx(idx+offset, len);
end;

constructor TVCCS.Create;  // Creates superstructure for all Line objects
Begin
     Inherited Create;
     Class_Name := 'VCCS';
     DSSClassType := VCCS_ELEMENT + PC_ELEMENT; // participates in dynamics

     ActiveElement := 0;

     DefineProperties;

     CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
     CommandList.Abbrev := TRUE;
     XY_CurveClass := GetDSSClassPtr('XYCurve');

     VCCSClass := Self;
End;

Destructor TVCCS.Destroy;
Begin
    Inherited Destroy;
End;

Procedure TVCCS.DefineProperties;
Begin
     NumPropsThisClass := 13;

     Numproperties := NumPropsThisClass;
     CountProperties;   // Get inherited property count
     AllocatePropertyArrays;

     // Define Property names
     PropertyName[1] := 'bus1';
     PropertyName[2] := 'phases';
     PropertyName[3] := 'prated';
     PropertyName[4] := 'vrated';
     PropertyName[5] := 'ppct';
     PropertyName[6] := 'bp1';
     PropertyName[7] := 'bp2';
     PropertyName[8] := 'filter';
     PropertyName[9] := 'fsample';
     PropertyName[10] := 'rmsmode';
     PropertyName[11] := 'imaxpu';
     PropertyName[12] := 'vrmstau';
     PropertyName[13] := 'irmstau';

     // define Property help values
     PropertyHelp[1] := 'Name of bus to which source is connected.'+CRLF+'bus1=busname'+CRLF+'bus1=busname.1.2.3';
     PropertyHelp[2] := 'Number of phases.  Defaults to 1.';
     PropertyHelp[3] := 'Total rated power, in Watts.';
     PropertyHelp[4] := 'Rated line-to-line voltage, in Volts';
     PropertyHelp[5] := 'Steady-state operating output, in percent of rated.';
     PropertyHelp[6] := 'XYCurve defining the input piece-wise linear block.';
     PropertyHelp[7] := 'XYCurve defining the output piece-wise linear block.';
     PropertyHelp[8] := 'XYCurve defining the digital filter coefficients (x numerator, y denominator).';
     PropertyHelp[9] := 'Sample frequency [Hz} for the digital filter.';
     PropertyHelp[10]:= 'True if only Hz is used to represent a phase-locked loop (PLL), ignoring the BP1, BP2 and time-domain transformations. Default is no.';
     PropertyHelp[11]:= 'Maximum output current in per-unit of rated; defaults to 1.1';
     PropertyHelp[12]:= 'Time constant in sensing Vrms for the PLL; defaults to 0.0015';
     PropertyHelp[13]:= 'Time constant in producing Irms from the PLL; defaults to 0.0015';

     ActiveProperty := NumPropsThisClass;
     inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
     PropertyHelp[NumPropsThisClass+1] := 'Harmonic spectrum assumed for this source.  Default is "default".';
End;

Function TVCCS.NewObject(const ObjName:String):Integer;
Begin
    With ActiveCircuit Do Begin
      ActiveCktElement := TVCCSObj.Create(Self, ObjName);
      Result := AddObjectToList(ActiveDSSObject);
    End;
End;

Function TVCCS.Edit:Integer;
VAR
   ParamPointer :Integer;
   ParamName,
   Param        :String;
Begin
  // continue parsing with contents of Parser
  ActiveVCCSObj            := ElementList.Active;
  ActiveCircuit.ActiveCktElement := ActiveVCCSObj;
  Result := 0;

  WITH ActiveVCCSObj DO Begin
     ParamPointer := 0;
     ParamName := Parser.NextParam;
     Param     := Parser.StrValue;
     WHILE Length(Param) > 0 DO Begin
         IF Length(ParamName) = 0 THEN Inc(ParamPointer)
         ELSE ParamPointer := CommandList.GetCommand(ParamName);

         If (ParamPointer > 0) and (ParamPointer <= NumProperties) Then PropertyValue[ParamPointer] := Param;

         CASE ParamPointer OF
            0: DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name +'.'+ Name + '"', 330);
            1: SetBus(1, param);
            2: Begin
                 Nphases   := Parser.IntValue; // num phases
                 NConds    := Fnphases;  // Force Reallocation of terminal info
               End;
            3: Prated := Parser.DblValue;
            4: Vrated := Parser.DblValue;
            5: Ppct := Parser.DblValue;
            6: Begin
                  Fbp1_name := Parser.StrValue;
                  if Length(Fbp1_name) > 0 then begin
                      Fbp1 := XY_CurveClass.Find(Fbp1_name);
                  end;
               End;
            7: Begin
                  Fbp2_name := Parser.StrValue;
                  if Length(Fbp2_name) > 0 then begin
                      Fbp2 := XY_CurveClass.Find(Fbp2_name);
                  end;
               End;
            8: Begin
                  Ffilter_name := Parser.StrValue;
                  if Length(Ffilter_name) > 0 then begin
                      Ffilter := XY_CurveClass.Find(Ffilter_name);
                  end;
               End;
            9: FsampleFreq := Parser.DblValue;
            10: FrmsMode := InterpretYesNo(Param);
            11: FmaxIpu := Parser.DblValue;
            12: FvrmsTau := Parser.DblValue;
            13: FirmsTau := Parser.DblValue;
         ELSE
            ClassEdit(ActiveVCCSObj, ParamPointer - NumPropsThisClass)
         End;
         ParamName := Parser.NextParam;
         Param     := Parser.StrValue;
     End;
     RecalcElementData;
     YPrimInvalid := True;
  End;
End;

//----------------------------------------------------------------------------
Function TVCCS.MakeLike(Const OtherSource:String):Integer;
var
  OtherVCCS :TVCCSObj;
  i :Integer;
Begin
  Result := 0;
  {See if we can find this line name in the present collection}
  OtherVCCS := Find(OtherSource);
  IF   OtherVCCS <> Nil THEN
    WITH ActiveVCCSObj DO Begin
      IF Fnphases <> OtherVCCS.Fnphases THEN Begin
        Nphases := OtherVCCS.Fnphases;
        NConds  := Fnphases;  // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := True;
      End;
      Prated := OtherVCCS.Prated;
      Vrated := OtherVCCS.Vrated;
      Ppct := OtherVCCS.Ppct;
      Fbp1 := OtherVCCS.Fbp1;
      Fbp2 := OtherVCCS.Fbp2;
      Ffilter := OtherVCCS.Ffilter;
      Fbp1_name := OtherVCCS.Fbp1_name;
      Fbp2_name := OtherVCCS.Fbp2_name;
      Ffilter_name := OtherVCCS.Ffilter_name;
      FsampleFreq := OtherVCCS.FsampleFreq;
      FrmsMode := OtherVCCS.FrmsMode;
      FmaxIpu := OtherVCCS.FmaxIpu;
      FvrmsTau := OtherVCCS.FvrmsTau;
      FirmsTau := OtherVCCS.FirmsTau;

      ClassMakeLike(OtherVCCS); // set spectrum,  base frequency

      For i := 1 to ParentClass.NumProperties Do PropertyValue[i] := OtherVCCS.PropertyValue[i];
      Result := 1;
    End
  ELSE DoSimpleMsg('Error in VCCS MakeLike: "' + OtherSource + '" Not Found.', 332);
End;

Function TVCCS.Init(Handle:Integer):Integer;
Begin
  DoSimpleMsg('Need to implement TVCCS.Init', -1);
  Result := 0;
End;

Constructor TVCCSObj.Create(ParClass:TDSSClass; const SourceName:String);
Begin
  Inherited create(ParClass);
  Name := LowerCase(SourceName);
  DSSObjType := ParClass.DSSClassType;

  Nphases := 1;
  Fnconds := 1;
  Nterms  := 1;

  Prated := 250.0;
  Vrated := 208.0;
  Ppct := 100.0;
  FsampleFreq := 5000.0;
  Fkv := 1.0;
  Fki := 1.0;
  FrmsMode := FALSE;
  FmaxIpu := 1.1;
  FvrmsTau := 0.0015;
  FirmsTau := 0.0015;

  Fwinlen := 0;
  Ffilter_name := '';
  Fbp1_name := '';
  Fbp2_name := '';
  y2 := nil;
  z := nil;
  whist := nil;
  zlast := nil;
  wlast := nil;

  InitPropertyValues(0);

  Yorder := Fnterms * Fnconds;
  RecalcElementData;
End;

Destructor TVCCSObj.Destroy;
Begin
  Reallocmem (y2, 0);
  Reallocmem (z, 0);
  Reallocmem (whist, 0);
  Reallocmem (wlast, 0);
  Reallocmem (zlast, 0);
  Inherited Destroy;
End;

Procedure TVCCSObj.RecalcElementData;
Begin
  SpectrumObj := SpectrumClass.Find(Spectrum);
  if SpectrumObj=NIL Then Begin
    DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device VCCS.'+Name+' Not Found.', 333);
  end;
  Reallocmem(InjCurrent, SizeOf(InjCurrent^[1])*Yorder);

  Irated := Prated / Vrated / FNphases;
  BaseVolt := Vrated;
  if FNPhases = 3 then begin
    Irated := Irated * sqrt(3);
    BaseVolt := BaseVolt / sqrt(3);
  end;
  BaseCurr := 0.01 * Ppct * Irated;
  Fkv := 1.0 / BaseVolt / sqrt(2.0);
  Fki := BaseCurr * sqrt(2.0);

  if Length (Ffilter_name) > 0 then begin
    Ffiltlen := Ffilter.NumPoints;
    Fwinlen := Trunc (FsampleFreq / BaseFrequency);
    Reallocmem (y2, sizeof(y2^[1]) * Fwinlen);
    Reallocmem (z, sizeof(z^[1]) * Ffiltlen);
    Reallocmem (whist, sizeof(whist^[1]) * Ffiltlen);
    Reallocmem (wlast, sizeof(wlast^[1]) * Ffiltlen);
    Reallocmem (zlast, sizeof(zlast^[1]) * Ffiltlen);
  end;
End;

Procedure TVCCSObj.CalcYPrim;
Begin
  // Build only YPrim Series
  IF YPrimInvalid THEN Begin
    IF YPrim_Series <> nil Then YPrim_Series.Free;
    YPrim_Series := TcMatrix.CreateMatrix(Yorder);
    IF YPrim <> nil Then YPrim.Free;
    YPrim := TcMatrix.CreateMatrix(Yorder);
  End ELSE Begin
    YPrim_Series.Clear;
    YPrim.Clear;
  End;
  {Yprim = 0  for Ideal Current Source;  just leave it zeroed}

  {Now Account for Open Conductors}
  {For any conductor that is open, zero out row and column}
  Inherited CalcYPrim;
  YPrimInvalid := False;
End;

Function TVCCSObj.InjCurrents:Integer;
{Sum Currents directly into solution array}
Begin
  GetInjCurrents(InjCurrent);
  Result := Inherited Injcurrents;  // Adds into system array
End;

Procedure TVCCSObj.GetCurrents(Curr: pComplexArray);
{Total currents into a device}
var
  i:Integer;
Begin
  try
    GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
    // Add Together with yprim currents
    for i := 1 to Yorder do Curr^[i] := Cnegate(ComplexBuffer^[i]);
  except
    On E: Exception
      Do DoErrorMsg(('GetCurrents for VCCS Element: ' + Name + '.'), E.Message,
        'Inadequate storage allotted for circuit element?', 335);
  End;
End;

Procedure TVCCSObj.UpdateSequenceVoltage;
begin
  if FNPhases = 3 then
    sV1 := cdivreal (cadd (Vterminal^[1], cadd (cmul(ALPHA1,Vterminal^[2]), cmul(ALPHA2,Vterminal^[3]))), 3.0)
  else
    sV1 := Vterminal^[1];
end;

Procedure TVCCSObj.GetInjCurrents(Curr:pComplexArray);
var
  i: Integer;
  i1: complex;
Begin
  if not Closed[1] then begin
    for i := 1 to Fnphases do Curr^[i] := CZERO;
    exit;
  end;
  ComputeVterminal;
  UpdateSequenceVoltage;
//  IterminalUpdated := FALSE;
  if ActiveSolutionObj.IsDynamicModel then begin
    if FrmsMode then begin
      i1 := pdegtocomplex (s4 * BaseCurr, cdang (sV1));
      case Fnphases of
        1: Curr^[1] := i1;
        3: begin
          Curr^[1] := i1;
          Curr^[2] := cmul (i1, ALPHA2);
          Curr^[3] := cmul (i1, ALPHA1);
        end;
      else
        For i := 1 to Fnphases Do Begin
          Curr^[i] := pdegtocomplex (s4 * BaseCurr, cdang(Vterminal^[i]));
        End;
      end;
    end else begin
      For i := 1 to Fnphases Do Begin
        Curr^[i] := pdegtocomplex (s3 * BaseCurr, cdang(Vterminal^[i]));
      End;
    end;
  end else begin
    For i := 1 to Fnphases Do Begin
      Curr^[i] := pdegtocomplex (BaseCurr, cdang(Vterminal^[i]));
    End;
  end;
End;

Procedure TVCCSObj.DumpProperties(Var F:TextFile; Complete:Boolean);
var
  i:Integer;
Begin
  Inherited DumpProperties(F,Complete);
  With ParentClass Do
    For i := 1 to NumProperties Do Begin
      Writeln(F,'~ ',PropertyName^[i],'=',PropertyValue[i]);
    End;
  If Complete Then Begin
    Writeln(F);
    Writeln(F);
  End;
End;

procedure TVCCSObj.InitPropertyValues(ArrayOffset: Integer);
begin
  PropertyValue[1] := GetBus(1);
  PropertyValue[2] := '1';
  PropertyValue[3] := '250';
  PropertyValue[4] := '208';
  PropertyValue[5] := '100';
  PropertyValue[6] := 'NONE';
  PropertyValue[7] := 'NONE';
  PropertyValue[8] := 'NONE';
  PropertyValue[9] := '5000';
  PropertyValue[10]:= 'no';
  PropertyValue[11]:= '1.1';
  PropertyValue[12]:= '0.0015';
  PropertyValue[13]:= '0.0015';
  inherited  InitPropertyValues(NumPropsThisClass);
end;

procedure TVCCSObj.MakePosSequence;
begin
  If Fnphases>1 Then Begin
    Parser.CmdString := 'phases=1';
    Edit;
  End;
  inherited;
end;

// support for DYNAMICMODE
// NB: in phasor mode, use load convention for OpenDSS
procedure TVCCSObj.ShutoffInjections; // stop injecting if the terminal opens
var
  i: integer;
begin
  for i:= 1 to FFiltlen do begin
    whist[i] := 0.0;
    wlast[i] := 0.0;
    z[i] := 0.0;
    zlast[i] := 0.0;
  end;
  for i:= 1 to FWinlen do y2[i] := 0.0;
  s1 := 0;
  s2 := 0;
  s3 := 0;
  s4 := 0;
  s5 := 0;
  s6 := 0;
end;

procedure TVCCSObj.InitPhasorStates;
var
  i, k: integer;
begin
  ComputeIterminal;
  s1 := cabs(Vterminal^[1]) / BaseVolt;
  s4 := cabs(Iterminal^[1]) / BaseCurr;
  s2 := s4;
  s3 := s4;
  s5 := 0;
  s6 := 0;
  sV1 := cmplx (1.0, 0.0);
  vlast := cdivreal (Vterminal^[1], BaseVolt);

  // initialize the history terms for HW model source convention
  for i := 1 to Ffiltlen do begin
    whist[i] := s1;
    wlast[i] := s1;
  end;
  for i := 1 to Fwinlen do begin
    k := i - Fwinlen + Ffiltlen;
    if k > 0 then begin
      z[k] := s4; // HW history with load convention
      zlast[k] := z[k];
    end;
  end;
  // initialize the ring buffer indices; these increment by 1 before actual use
  sIdxU := 0;
  sIdxY := 0;
end;

// support for DYNAMICMODE
// NB: The test data and HW model used source convention (I and V in phase)
//     However, OpenDSS uses the load convention
procedure TVCCSObj.InitStateVars;
var
  d, wt, wd, val, iang, vang: double;
  i, k: integer;
begin
  // initialize outputs from the terminal conditions
  if FrmsMode then begin
    InitPhasorStates;
    exit;
  end;
  ComputeIterminal;
  iang := cang(Iterminal^[1]);
  vang := cang(Vterminal^[1]);
  s1 := cabs(Vterminal^[1]) / BaseVolt;
  s3 := cabs(Iterminal^[1]) / BaseCurr;
  s2 := s3;
  s4 := s3;
  s5 := 0;
  s6 := 0;
  sV1 := cmplx (1.0, 0.0);
  vlast := cdivreal (Vterminal^[1], BaseVolt);

  // initialize the history terms for HW model source convention
  d := 1 / FsampleFreq;
  wd := 2 * Pi * ActiveSolutionObj.Frequency * d;
  for i := 1 to Ffiltlen do begin
    wt := vang - wd * (Ffiltlen - i);
    whist[i] := 0;
    whist[i] := Fbp1.GetYValue(s1 * cos(wt));
    wlast[i] := whist[i];
  end;
  for i := 1 to Fwinlen do begin
    wt := iang - wd * (Fwinlen - i);
    val := s3 * cos(wt);  // current by passive sign convention
    y2[i] := val * val;
    k := i - Fwinlen + Ffiltlen;
    if k > 0 then begin
      z[k] := -Fbp2.GetXvalue (val); // HW history with generator convention
      zlast[k] := z[k];
    end;
  end;

  // initialize the ring buffer indices; these increment by 1 before actual use
  sIdxU := 0;
  sIdxY := 0;
end;

procedure TVCCSObj.IntegratePhasorStates;
var
  vpu, ipwr, imax, h, d: double;
  iu, i, k, nstep, corrector: integer;
begin
  ComputeIterminal;
  UpdateSequenceVoltage;
  vpu := cabs (sV1) / BaseVolt;
  if vpu > 0.0 then begin
    h := ActiveSolutionObj.DynaVars.h;
    corrector := ActiveSolutionObj.DynaVars.IterationFlag;
    nstep := trunc (1e-6 + h * FSampleFreq);
    // Vrms from LPF
    d := vpu - s1;
    s1 := s1 + d * (1.0 - exp(-h/FvrmsTau));
    // rms current to maintain power
    ipwr := BaseCurr / s1;
    imax := FmaxIpu * Irated;
    if ipwr > imax then ipwr := imax;
    s2 := ipwr / BaseCurr;
    // Hout
//    s3 := s2;
    iu := sIdxU;
    for k := 1 to FFiltlen do begin
      z[k] := zlast[k];
      whist[k] := wlast[k];
    end;
    for i:=1 to nstep do begin
      iu := OffsetIdx (iu, 1, Ffiltlen);
      whist[iu] := s2;
      // apply the filter and second PWL block
      z[iu] := 0;
      for k := 1 to Ffiltlen do begin
        z[iu] := z[iu] + Ffilter.Yvalue_pt[k] * whist[MapIdx(iu-k+1,Ffiltlen)];
      end;
      for k := 2 to Ffiltlen do begin
        z[iu] := z[iu] - Ffilter.Xvalue_pt[k] * z[MapIdx(iu-k+1,Ffiltlen)];
      end;
      s3 := z[iu];
    end;
    // Irms through LPF
    d := s3 - s4;
    s4 := s4 + d * (1.0 - exp(-h/FirmsTau));
    if corrector = 1 then begin
      sIdxU := iu;
      for k := 1 to FFiltlen do begin
        zlast[k] := z[k];
        wlast[k] := whist[k];
      end;
    end;
  end;
end;

// this is called twice per dynamic time step; predictor then corrector
procedure TVCCSObj.IntegrateStates;
var
  t, h, d, f, w, wt: double;
  vre, vim, vin, scale, y: double;
  nstep, i, k, corrector: integer;
  vnow: complex;
  iu, iy: integer; // local copies of sIdxU and sIdxY for predictor
begin
  if not Closed[1] then begin
    ShutoffInjections;
    exit;
  end;
  if FrmsMode then begin
    IntegratePhasorStates;
    exit;
  end;

  ComputeIterminal;

  t := ActiveSolutionObj.DynaVars.t;
  h := ActiveSolutionObj.DynaVars.h;
  f := ActiveSolutionObj.Frequency;
  corrector := ActiveSolutionObj.DynaVars.IterationFlag;
  d := 1 / FSampleFreq;
  nstep := trunc (1e-6 + h/d);
  w := 2 * Pi * f;

  vnow := cdivreal (Vterminal^[1], BaseVolt);
  vin := 0;
  y := 0;
  iu := sIdxU;
  iy := sIdxY;
  for k := 1 to FFiltlen do begin
    z[k] := zlast[k];
    whist[k] := wlast[k];
  end;
  for i:=1 to nstep do begin
    iu := OffsetIdx (iu, 1, Ffiltlen);
    // push input voltage waveform through the first PWL block
    scale := 1.0 * i / nstep;
    vre := vlast.re + (vnow.re - vlast.re) * scale;
    vim := vlast.im + (vnow.im - vlast.im) * scale;
    wt := w * (t - h + i * d);
    vin := (vre * cos(wt) + vim * sin(wt));
    whist[iu] := Fbp1.GetYValue(vin);
    // apply the filter and second PWL block
    z[iu] := 0;
    for k := 1 to Ffiltlen do begin
      z[iu] := z[iu] + Ffilter.Yvalue_pt[k] * whist[MapIdx(iu-k+1,Ffiltlen)];
    end;
    for k := 2 to Ffiltlen do begin
      z[iu] := z[iu] - Ffilter.Xvalue_pt[k] * z[MapIdx(iu-k+1,Ffiltlen)];
    end;
    y := Fbp2.GetYValue(z[iu]);
    // updating outputs
    if (corrector = 1) and (abs(y) > s4) then
      s4 := abs(y); // catching the fastest peaks
    // update the RMS
    iy := OffsetIdx (iy, 1, Fwinlen);
    y2[iy] := y * y;  // brute-force RMS update
    if i = nstep then begin
      y2sum := 0.0;
      for k := 1 to Fwinlen do y2sum := y2sum + y2[k];
      s3 := sqrt(2.0 * y2sum / Fwinlen); // TODO - this is the magnitude, what about angle?
    end;
  end;

  if corrector = 1 then begin
    sIdxU := iu;
    sIdxY := iy;
    vlast := vnow;
    s1 := vin;
    s5 := whist[sIdxU];
    s6 := z[sIdxU];
    s2 := y;
    for k := 1 to FFiltlen do begin
      zlast[k] := z[k];
      wlast[k] := whist[k];
    end;
  end;
end;

function TVCCSObj.NumVariables: Integer;
begin
  Result := 6;
end;

procedure TVCCSObj.GetAllVariables( States: pDoubleArray);
var
  i: integer;
begin
  for i := 1 to 6 Do States^[i] := Variable[i];  // property maps to Get_Variable below
end;

Function TVCCSObj.VariableName(i: Integer):String;
begin
  Result := '';
  if FrmsMode then begin
    case i of
      1: Result := 'Vrms';
      2: Result := 'Ipwr';
      3: Result := 'Hout';
      4: Result := 'Irms';
      5: Result := 'NA';
      6: Result := 'NA';
    end;
  end else begin
    case i of
      1: Result := 'Vwave';
      2: Result := 'Iwave';
      3: Result := 'Irms';
      4: Result := 'Ipeak';
      5: Result := 'BP1out';
      6: Result := 'Hout';
    end;
  end;
end;

function TVCCSObj.Get_Variable(i: Integer): Double;
begin
  Result := 0;
  case i of
    1: Result := s1;
    2: Result := s2;
    3: Result := s3;
    4: Result := s4;
    5: Result := s5;
    6: Result := s6;
  end;
end;

procedure TVCCSObj.Set_Variable(i: Integer;  Value: Double);
begin
  case i of
    1: s1 := Value;
    2: s2 := Value;
    3: s3 := Value;
    4: s4 := Value;
    5: s5 := Value;
    6: s6 := Value;
  end;
end;

initialization
  ALPHA1 := cmplx (-0.5, 0.5 * sqrt(3.0));  // 1 at 120 degrees
  ALPHA2 := cmplx (-0.5, -ALPHA1.im);       // 1 at 240 degrees
end.

