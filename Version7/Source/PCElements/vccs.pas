unit VCCS;

{
  ----------------------------------------------------------
  Copyright (c) 2016, University of Pittsburgh
  Copyright (c) 2019, Battelle Memorial Institute
  All rights reserved.
  ----------------------------------------------------------
}

{$ifdef fpc}{$mode delphi}{$endif}

interface

uses
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    ucomplex,
    XYCurve,
    ArrayDef;

type
    TVCCS = class(TPCClass)
    PRIVATE
        XY_CurveClass: TDSSClass;
    PROTECTED
        procedure DefineProperties;
        function MakeLike(const OtherSource: String): Integer; OVERRIDE;
    PUBLIC
        constructor Create(dss: TDSS);
        destructor Destroy; OVERRIDE;

        function Edit: Integer; OVERRIDE;
        function Init(Handle: Integer): Integer; OVERRIDE;
        function NewObject(const ObjName: String): Integer; OVERRIDE;
    end;

    TVCCSObj = class(TPCElement)
    PRIVATE
        Fbp1: TXYcurveObj;
        Fbp1_name: String;
        Fbp2: TXYcurveObj;
        Fbp2_name: String;
        Ffilter: TXYcurveObj;
        Ffilter_name: String;
        BaseCurr: Double; // line current at Ppct
        FsampleFreq: Double; // discretization frequency for Z filter
        Fwinlen: Integer;
        Ffiltlen: Integer;
        Irated: Double; // line current at full output
        Fkv: Double; // scale voltage to HW pu input
        Fki: Double; // scale HW pu output to current

        FrmsMode: Boolean; // indicates a phasor-domain PLL simulation
        FmaxIpu: Double; // maximum RMS current in per-unit of rated
        FvrmsTau: Double; // LPF time constant sensing Vrms
        FirmsTau: Double; // LPF time constant producing Irms

        // Support for Dynamics Mode - PU of Vrated and BaseCurr
        // state variables for Dynamics Mode
        s1: Double; // Vwave(t), or Vrms in phasor mode
        s2: Double; // Iwave(t), or Ipwr in phasor mode
        s3: Double; // Irms,     or Hout in phasor mode
        s4: Double; // Ipeak,    or Irms in phasor mode
        s5: Double; // BP1out,   or NA in phasor mode
        s6: Double; // Hout,     or NA in phasor mode

        vlast: complex;
        y2: pDoubleArray;
        z: pDoubleArray;     // current digital filter history terms
        whist: pDoubleArray;
        zlast: pDoubleArray; // update only after the corrector step
        wlast: pDoubleArray;
        sIdxU: Integer; // ring buffer index for z and whist
        sIdxY: Integer; // ring buffer index for y2 (rms current)
        y2sum: Double;
        procedure InitPhasorStates;
        procedure GetInjCurrents(Curr: pComplexArray);
        procedure IntegratePhasorStates;

    PROTECTED
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
    PUBLIC
        Ppct, Prated, Vrated: Double;
        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence; OVERRIDE;  // Make a positive Sequence Model

        function InjCurrents: Integer; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        procedure InitPropertyValues(ArrayOffset: Integer); OVERRIDE;
        procedure DumpProperties(var F: TextFile; Complete: Boolean); OVERRIDE;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;
    end;

var
    ActiveVCCSObj: TVCCSObj;

implementation

uses
    ParserDel,
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command,
    Solution,
    DSSHelper;

var
    NumPropsThisClass: Integer;

// helper functions for ring buffer indexing, 1..len
function MapIdx(idx, len: Integer): Integer;
begin
    while idx <= 0 do
        idx := idx + len;
    Result := idx mod (len + 1);
    if Result = 0 then
        Result := 1;
end;

function OffsetIdx(idx, offset, len: Integer): Integer;
begin
    Result := MapIdx(idx + offset, len);
end;

constructor TVCCS.Create(dss: TDSS);  // Creates superstructure for all Line objects
begin
    inherited Create(dss);
    Class_Name := 'VCCS';
    DSSClassType := VCCS_ELEMENT + PC_ELEMENT; // participates in dynamics

    ActiveElement := 0;

    DefineProperties;

    CommandList := TCommandList.Create(Slice(PropertyName^, NumProperties));
    CommandList.Abbrev := TRUE;
    XY_CurveClass := GetDSSClassPtr(DSS, 'XYCurve');
end;

destructor TVCCS.Destroy;
begin
    inherited Destroy;
end;

procedure TVCCS.DefineProperties;
begin
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
    PropertyHelp[1] := 'Name of bus to which source is connected.' + CRLF + 'bus1=busname' + CRLF + 'bus1=busname.1.2.3';
    PropertyHelp[2] := 'Number of phases.  Defaults to 1.';
    PropertyHelp[3] := 'Total rated power, in Watts.';
    PropertyHelp[4] := 'Rated line-to-line voltage, in Volts';
    PropertyHelp[5] := 'Steady-state operating output, in percent of rated.';
    PropertyHelp[6] := 'XYCurve defining the input piece-wise linear block.';
    PropertyHelp[7] := 'XYCurve defining the output piece-wise linear block.';
    PropertyHelp[8] := 'XYCurve defining the digital filter coefficients (x numerator, y denominator).';
    PropertyHelp[9] := 'Sample frequency [Hz} for the digital filter.';
    PropertyHelp[10] := 'True if only Hz is used to represent a phase-locked loop (PLL), ignoring the BP1, BP2 and time-domain transformations. Default is no.';
    PropertyHelp[11] := 'Maximum output current in per-unit of rated; defaults to 1.1';
    PropertyHelp[12] := 'Time constant in sensing Vrms for the PLL; defaults to 0.0015';
    PropertyHelp[13] := 'Time constant in producing Irms from the PLL; defaults to 0.0015';

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;  // Add defs of inherited properties to bottom of list

     // Override help string
    PropertyHelp[NumPropsThisClass + 1] := 'Harmonic spectrum assumed for this source.  Default is "default".';
end;

function TVCCS.NewObject(const ObjName: String): Integer;
begin
    with DSS.ActiveCircuit do
    begin
        ActiveCktElement := TVCCSObj.Create(Self, ObjName);
        Result := AddObjectToList(ActiveDSSObject);
    end;
end;

function TVCCS.Edit: Integer;
var
    ParamPointer: Integer;
    ParamName,
    Param: String;
begin
  // continue parsing with contents of Parser
    ActiveVCCSObj := ElementList.Active;
    DSS.ActiveCircuit.ActiveCktElement := ActiveVCCSObj;
    Result := 0;

    with ActiveVCCSObj do
    begin
        ParamPointer := 0;
        ParamName := Parser.NextParam;
        Param := Parser.StrValue;
        while Length(Param) > 0 do
        begin
            if Length(ParamName) = 0 then
                Inc(ParamPointer)
            else
                ParamPointer := CommandList.GetCommand(ParamName);

            if (ParamPointer > 0) and (ParamPointer <= NumProperties) then
                PropertyValue[ParamPointer] := Param;

            case ParamPointer of
                0:
                    DoSimpleMsg('Unknown parameter "' + ParamName + '" for Object "' + Class_Name + '.' + Name + '"', 330);
                1:
                    SetBus(1, param);
                2:
                begin
                    Nphases := Parser.IntValue; // num phases
                    NConds := Fnphases;  // Force Reallocation of terminal info
                end;
                3:
                    Prated := Parser.DblValue;
                4:
                    Vrated := Parser.DblValue;
                5:
                    Ppct := Parser.DblValue;
                6:
                begin
                    Fbp1_name := Parser.StrValue;
                    if Length(Fbp1_name) > 0 then
                    begin
                        Fbp1 := XY_CurveClass.Find(Fbp1_name);
                    end;
                end;
                7:
                begin
                    Fbp2_name := Parser.StrValue;
                    if Length(Fbp2_name) > 0 then
                    begin
                        Fbp2 := XY_CurveClass.Find(Fbp2_name);
                    end;
                end;
                8:
                begin
                    Ffilter_name := Parser.StrValue;
                    if Length(Ffilter_name) > 0 then
                    begin
                        Ffilter := XY_CurveClass.Find(Ffilter_name);
                    end;
                end;
                9:
                    FsampleFreq := Parser.DblValue;
                10:
                    FrmsMode := InterpretYesNo(Param);
                11:
                    FmaxIpu := Parser.DblValue;
                12:
                    FvrmsTau := Parser.DblValue;
                13:
                    FirmsTau := Parser.DblValue;
            else
                ClassEdit(ActiveVCCSObj, ParamPointer - NumPropsThisClass)
            end;
            ParamName := Parser.NextParam;
            Param := Parser.StrValue;
        end;
        RecalcElementData;
        YPrimInvalid := TRUE;
    end;
end;

//----------------------------------------------------------------------------
function TVCCS.MakeLike(const OtherSource: String): Integer;
var
    OtherVCCS: TVCCSObj;
    i: Integer;
begin
    Result := 0;
  {See if we can find this line name in the present collection}
    OtherVCCS := Find(OtherSource);
    if OtherVCCS <> NIL then
        with ActiveVCCSObj do
        begin
            if Fnphases <> OtherVCCS.Fnphases then
            begin
                Nphases := OtherVCCS.Fnphases;
                NConds := Fnphases;  // Forces reallocation of terminal stuff

                Yorder := Fnconds * Fnterms;
                YPrimInvalid := TRUE;
            end;
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

            for i := 1 to ParentClass.NumProperties do
                PropertyValue[i] := OtherVCCS.PropertyValue[i];
            Result := 1;
        end
    else
        DoSimpleMsg('Error in VCCS MakeLike: "' + OtherSource + '" Not Found.', 332);
end;

function TVCCS.Init(Handle: Integer): Integer;
begin
    DoSimpleMsg('Need to implement TVCCS.Init', -1);
    Result := 0;
end;

constructor TVCCSObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType;

    Nphases := 1;
    Fnconds := 1;
    Nterms := 1;

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
    y2 := NIL;
    z := NIL;
    whist := NIL;
    zlast := NIL;
    wlast := NIL;

    InitPropertyValues(0);

    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

destructor TVCCSObj.Destroy;
begin
    Reallocmem(y2, 0);
    Reallocmem(z, 0);
    Reallocmem(whist, 0);
    Reallocmem(wlast, 0);
    Reallocmem(zlast, 0);
    inherited Destroy;
end;

procedure TVCCSObj.RecalcElementData;
begin
    SpectrumObj := DSSPrime.SpectrumClass.Find(Spectrum);
    if SpectrumObj = NIL then
    begin
        DoSimpleMsg('Spectrum Object "' + Spectrum + '" for Device VCCS.' + Name + ' Not Found.', 333);
    end;
    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    Irated := Prated / Vrated / FNphases;
    if FNPhases = 3 then
        Irated := Irated * sqrt(3);
    BaseCurr := 0.01 * Ppct * Irated;
    Fkv := 1.0 / Vrated / sqrt(2.0);
    Fki := BaseCurr * sqrt(2.0);

    if Length(Ffilter_name) > 0 then
    begin
        Ffiltlen := Ffilter.NumPoints;
        Fwinlen := Trunc(FsampleFreq / BaseFrequency);
        Reallocmem(y2, sizeof(y2^[1]) * Fwinlen);
        Reallocmem(z, sizeof(z^[1]) * Ffiltlen);
        Reallocmem(whist, sizeof(whist^[1]) * Ffiltlen);
        Reallocmem(wlast, sizeof(wlast^[1]) * Ffiltlen);
        Reallocmem(zlast, sizeof(zlast^[1]) * Ffiltlen);
    end;
end;

procedure TVCCSObj.CalcYPrim;
begin
  // Build only YPrim Series
    if YPrimInvalid then
    begin
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;
        YPrim.Clear;
    end;
  {Yprim = 0  for Ideal Current Source;  just leave it zeroed}

  {Now Account for Open Conductors}
  {For any conductor that is open, zero out row and column}
    inherited CalcYPrim;
    YPrimInvalid := FALSE;
end;

function TVCCSObj.InjCurrents: Integer;
{Sum Currents directly into solution array}
begin
    GetInjCurrents(InjCurrent);
    Result := inherited Injcurrents;  // Adds into system array
end;

procedure TVCCSObj.GetCurrents(Curr: pComplexArray);
{Total currents into a device}
var
    i: Integer;
begin
    try
        GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
    // Add Together with yprim currents
        for i := 1 to Yorder do
            Curr^[i] := Cnegate(ComplexBuffer^[i]);
    except
        On E: Exception do
            DoErrorMsg(('GetCurrents for VCCS Element: ' + Name + '.'), E.Message,
                'Inadequate storage allotted for circuit element?', 335);
    end;
end;

procedure TVCCSObj.GetInjCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    ComputeVterminal;
//  IterminalUpdated := FALSE;
    if ActiveSolutionObj.IsDynamicModel then
    begin
        if FrmsMode then
        begin
            for i := 1 to Fnphases do
            begin
                Curr^[i] := pdegtocomplex(s4 * BaseCurr, cdang(Vterminal^[i]));
            end;
        end
        else
        begin
            for i := 1 to Fnphases do
            begin
                Curr^[i] := pdegtocomplex(s3 * BaseCurr, cdang(Vterminal^[i]));
            end;
        end;
    end
    else
    begin
        for i := 1 to Fnphases do
        begin
            Curr^[i] := pdegtocomplex(BaseCurr, cdang(Vterminal^[i]));
        end;
    end;
end;

procedure TVCCSObj.DumpProperties(var F: TextFile; Complete: Boolean);
var
    i: Integer;
begin
    inherited DumpProperties(F, Complete);
    with ParentClass do
        for i := 1 to NumProperties do
        begin
            Writeln(F, '~ ', PropertyName^[i], '=', PropertyValue[i]);
        end;
    if Complete then
    begin
        Writeln(F);
        Writeln(F);
    end;
end;

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
    PropertyValue[10] := 'no';
    PropertyValue[11] := '1.1';
    PropertyValue[12] := '0.0015';
    PropertyValue[13] := '0.0015';
    inherited  InitPropertyValues(NumPropsThisClass);
end;

procedure TVCCSObj.MakePosSequence;
begin
    if Fnphases > 1 then
    begin
        Parser.CmdString := 'phases=1';
        Edit;
    end;
    inherited;
end;

// support for DYNAMICMODE
// NB: in phasor mode, use load convention for OpenDSS
procedure TVCCSObj.InitPhasorStates;
var
    i, k: Integer;
begin
    ComputeIterminal;
    s1 := cabs(Vterminal^[1]) / Vrated;
    s4 := cabs(Iterminal^[1]) / BaseCurr;
    s2 := s4;
    s3 := s4;
    s5 := 0;
    s6 := 0;
    vlast := cdivreal(Vterminal^[1], Vrated);

  // initialize the history terms for HW model source convention
    for i := 1 to Ffiltlen do
    begin
        whist[i] := s1;
        wlast[i] := s1;
    end;
    for i := 1 to Fwinlen do
    begin
        k := i - Fwinlen + Ffiltlen;
        if k > 0 then
        begin
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
    d, wt, wd, val, iang, vang: Double;
    i, k: Integer;
begin
  // initialize outputs from the terminal conditions
    if FrmsMode then
    begin
        InitPhasorStates;
        exit;
    end;
    ComputeIterminal;
    iang := cang(Iterminal^[1]);
    vang := cang(Vterminal^[1]);
    s1 := cabs(Vterminal^[1]) / Vrated;
    s3 := cabs(Iterminal^[1]) / BaseCurr;
    s2 := s3;
    s4 := s3;
    s5 := 0;
    s6 := 0;
    vlast := cdivreal(Vterminal^[1], Vrated);

  // initialize the history terms for HW model source convention
    d := 1 / FsampleFreq;
    wd := 2 * Pi * ActiveSolutionObj.Frequency * d;
    for i := 1 to Ffiltlen do
    begin
        wt := vang - wd * (Ffiltlen - i);
        whist[i] := 0;
        whist[i] := Fbp1.GetYValue(s1 * cos(wt));
        wlast[i] := whist[i];
    end;
    for i := 1 to Fwinlen do
    begin
        wt := iang - wd * (Fwinlen - i);
        val := s3 * cos(wt);  // current by passive sign convention
        y2[i] := val * val;
        k := i - Fwinlen + Ffiltlen;
        if k > 0 then
        begin
            z[k] := -Fbp2.GetXvalue(val); // HW history with generator convention
            zlast[k] := z[k];
        end;
    end;

  // initialize the ring buffer indices; these increment by 1 before actual use
    sIdxU := 0;
    sIdxY := 0;
end;

procedure TVCCSObj.IntegratePhasorStates;
var
    vpu, ipwr, imax, h, d: Double;
    iu, i, k, nstep, corrector: Integer;
begin
    ComputeIterminal;
    vpu := cabs(Vterminal^[1]) / Vrated;
    if vpu > 0.0 then
    begin
        h := ActiveSolutionObj.DynaVars.h;
        corrector := ActiveSolutionObj.DynaVars.IterationFlag;
        nstep := trunc(1e-6 + h * FSampleFreq);
    // Vrms from LPF
        d := vpu - s1;
        s1 := s1 + d * (1.0 - exp(-h / FvrmsTau));
    // rms current to maintain power
        ipwr := BaseCurr / s1;
        imax := FmaxIpu * Irated;
        if ipwr > imax then
            ipwr := imax;
        s2 := ipwr / BaseCurr;
    // Hout
//    s3 := s2;
        iu := sIdxU;
        for k := 1 to FFiltlen do
        begin
            z[k] := zlast[k];
            whist[k] := wlast[k];
        end;
        for i := 1 to nstep do
        begin
            iu := OffsetIdx(iu, 1, Ffiltlen);
            whist[iu] := s2;
      // apply the filter and second PWL block
            z[iu] := 0;
            for k := 1 to Ffiltlen do
            begin
                z[iu] := z[iu] + Ffilter.Yvalue_pt[k] * whist[MapIdx(iu - k + 1, Ffiltlen)];
            end;
            for k := 2 to Ffiltlen do
            begin
                z[iu] := z[iu] - Ffilter.Xvalue_pt[k] * z[MapIdx(iu - k + 1, Ffiltlen)];
            end;
            s3 := z[iu];
        end;
    // Irms through LPF
        d := s3 - s4;
        s4 := s4 + d * (1.0 - exp(-h / FirmsTau));
        if corrector = 1 then
        begin
            sIdxU := iu;
            for k := 1 to FFiltlen do
            begin
                zlast[k] := z[k];
                wlast[k] := whist[k];
            end;
        end;
    end;
end;

// this is called twice per dynamic time step; predictor then corrector
procedure TVCCSObj.IntegrateStates;
var
    t, h, d, f, w, wt: Double;
    vre, vim, vin, scale, y: Double;
    nstep, i, k, corrector: Integer;
    vnow: complex;
    iu, iy: Integer; // local copies of sIdxU and sIdxY for predictor
begin
    if FrmsMode then
    begin
        IntegratePhasorStates;
        exit;
    end;

    ComputeIterminal;

    t := ActiveSolutionObj.DynaVars.t;
    h := ActiveSolutionObj.DynaVars.h;
    f := ActiveSolutionObj.Frequency;
    corrector := ActiveSolutionObj.DynaVars.IterationFlag;
    d := 1 / FSampleFreq;
    nstep := trunc(1e-6 + h / d);
    w := 2 * Pi * f;

    vnow := cdivreal(Vterminal^[1], Vrated);
    vin := 0;
    y := 0;
    iu := sIdxU;
    iy := sIdxY;
    for k := 1 to FFiltlen do
    begin
        z[k] := zlast[k];
        whist[k] := wlast[k];
    end;
    for i := 1 to nstep do
    begin
        iu := OffsetIdx(iu, 1, Ffiltlen);
    // push input voltage waveform through the first PWL block
        scale := 1.0 * i / nstep;
        vre := vlast.re + (vnow.re - vlast.re) * scale;
        vim := vlast.im + (vnow.im - vlast.im) * scale;
        wt := w * (t - h + i * d);
        vin := (vre * cos(wt) + vim * sin(wt));
        whist[iu] := Fbp1.GetYValue(vin);
    // apply the filter and second PWL block
        z[iu] := 0;
        for k := 1 to Ffiltlen do
        begin
            z[iu] := z[iu] + Ffilter.Yvalue_pt[k] * whist[MapIdx(iu - k + 1, Ffiltlen)];
        end;
        for k := 2 to Ffiltlen do
        begin
            z[iu] := z[iu] - Ffilter.Xvalue_pt[k] * z[MapIdx(iu - k + 1, Ffiltlen)];
        end;
        y := Fbp2.GetYValue(z[iu]);
    // updating outputs
        if (corrector = 1) and (abs(y) > s4) then
            s4 := abs(y); // catching the fastest peaks
    // update the RMS
        iy := OffsetIdx(iy, 1, Fwinlen);
        y2[iy] := y * y;  // brute-force RMS update
        if i = nstep then
        begin
            y2sum := 0.0;
            for k := 1 to Fwinlen do
                y2sum := y2sum + y2[k];
            s3 := sqrt(2.0 * y2sum / Fwinlen); // TODO - this is the magnitude, what about angle?
        end;
    end;

    if corrector = 1 then
    begin
        sIdxU := iu;
        sIdxY := iy;
        vlast := vnow;
        s1 := vin;
        s5 := whist[sIdxU];
        s6 := z[sIdxU];
        s2 := y;
        for k := 1 to FFiltlen do
        begin
            zlast[k] := z[k];
            wlast[k] := whist[k];
        end;
    end;
end;

function TVCCSObj.NumVariables: Integer;
begin
    Result := 6;
end;

procedure TVCCSObj.GetAllVariables(States: pDoubleArray);
var
    i: Integer;
begin
    for i := 1 to 6 do
        States^[i] := Variable[i];  // property maps to Get_Variable below
end;

function TVCCSObj.VariableName(i: Integer): String;
begin
    Result := '';
    if FrmsMode then
    begin
        case i of
            1:
                Result := 'Vrms';
            2:
                Result := 'Ipwr';
            3:
                Result := 'Hout';
            4:
                Result := 'Irms';
            5:
                Result := 'NA';
            6:
                Result := 'NA';
        end;
    end
    else
    begin
        case i of
            1:
                Result := 'Vwave';
            2:
                Result := 'Iwave';
            3:
                Result := 'Irms';
            4:
                Result := 'Ipeak';
            5:
                Result := 'BP1out';
            6:
                Result := 'Hout';
        end;
    end;
end;

function TVCCSObj.Get_Variable(i: Integer): Double;
begin
    Result := 0;
    case i of
        1:
            Result := s1;
        2:
            Result := s2;
        3:
            Result := s3;
        4:
            Result := s4;
        5:
            Result := s5;
        6:
            Result := s6;
    end;
end;

procedure TVCCSObj.Set_Variable(i: Integer; Value: Double);
begin
    case i of
        1:
            s1 := Value;
        2:
            s2 := Value;
        3:
            s3 := Value;
        4:
            s4 := Value;
        5:
            s5 := Value;
        6:
            s6 := Value;
    end;
end;

end.
