unit VCCS;

{
  ----------------------------------------------------------
  Copyright (c) 2016, University of Pittsburgh
  Copyright (c) 2019-2021, Battelle Memorial Institute
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    XYCurve,
    ArrayDef;

type
{$SCOPEDENUMS ON}
    TVCCSProp = (
        INVALID = 0,
        bus1 = 1,
        phases = 2,
        prated = 3,
        vrated = 4,
        ppct = 5,
        bp1 = 6,
        bp2 = 7,
        filter = 8,
        fsample = 9,
        rmsmode = 10,
        imaxpu = 11,
        vrmstau = 12,
        irmstau = 13
    );
{$SCOPEDENUMS OFF}

    TVCCS = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TVCCSObj = class(TPCElement)
    PRIVATE
        Fbp1: TXYcurveObj;
        Fbp2: TXYcurveObj;
        Ffilter: TXYcurveObj;
        BaseCurr: Double; // line current at Ppct
        BaseVolt: Double; // line-to-neutral voltage at Vrated
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
        sV1: Complex; // positive-sequence voltage; use to inject I1 only

        vlast: Complex;
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
        procedure ShutoffInjections;
        procedure UpdateSequenceVoltage;

    PROTECTED
        function Get_Variable(i: Integer): Double; OVERRIDE;
        procedure Set_Variable(i: Integer; Value: Double); OVERRIDE;
    PUBLIC
        Ppct, Prated, Vrated: Double;
        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        function InjCurrents: Integer; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        // Support for Dynamics Mode
        procedure InitStateVars; OVERRIDE;
        procedure IntegrateStates; OVERRIDE;
        function NumVariables: Integer; OVERRIDE;
        procedure GetAllVariables(States: pDoubleArray); OVERRIDE;
        function VariableName(i: Integer): String; OVERRIDE;
    end;


implementation

uses
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command,
    Solution,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TVCCSObj;
    TProp = TVCCSProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    
    ALPHA1, ALPHA2: complex;

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

constructor TVCCS.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, VCCS_ELEMENT, 'VCCS');
end;

destructor TVCCS.Destroy;
begin
    inherited Destroy;
end;

procedure TVCCS.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, False);

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // boolean properties
    PropertyType[ord(TProp.rmsmode)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.rmsmode)] := ptruint(@obj.FrmsMode);

    // object properties
    PropertyType[ord(TProp.bp1)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.bp2)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.filter)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.bp1)] := ptruint(@obj.Fbp1);
    PropertyOffset[ord(TProp.bp2)] := ptruint(@obj.Fbp2);
    PropertyOffset[ord(TProp.filter)] := ptruint(@obj.Ffilter);

    PropertyOffset2[ord(TProp.bp1)] := ptruint(DSS.XYCurveClass);
    PropertyOffset2[ord(TProp.bp2)] := ptruint(DSS.XYCurveClass);
    PropertyOffset2[ord(TProp.filter)] := ptruint(DSS.XYCurveClass);

    // double properties (default type)
    PropertyOffset[ord(TProp.prated)] := ptruint(@obj.Prated);
    PropertyOffset[ord(TProp.vrated)] := ptruint(@obj.Vrated);
    PropertyOffset[ord(TProp.ppct)] := ptruint(@obj.Ppct);
    PropertyOffset[ord(TProp.fsample)] := ptruint(@obj.FsampleFreq);
    PropertyOffset[ord(TProp.imaxpu)] := ptruint(@obj.FmaxIpu);
    PropertyOffset[ord(TProp.vrmstau)] := ptruint(@obj.FvrmsTau);
    PropertyOffset[ord(TProp.irmstau)] := ptruint(@obj.FirmsTau);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TVCCS.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TVCCSObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.phases):
            if FNPhases <> previousIntVal then
                NConds := Fnphases;  // Force Reallocation of terminal info
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TVCCS.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TVCCSObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);

    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        FNphases := Other.Fnphases;
        NConds := Fnphases;  // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;
    Prated := Other.Prated;
    Vrated := Other.Vrated;
    Ppct := Other.Ppct;
    Fbp1 := Other.Fbp1;
    Fbp2 := Other.Fbp2;
    Ffilter := Other.Ffilter;
    FsampleFreq := Other.FsampleFreq;
    FrmsMode := Other.FrmsMode;
    FmaxIpu := Other.FmaxIpu;
    FvrmsTau := Other.FvrmsTau;
    FirmsTau := Other.FirmsTau;
end;

constructor TVCCSObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType;

    FNphases := 1;
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

    Ffilter := NIL;
    Fbp1 := NIL;
    Fbp2 := NIL;

    y2 := NIL;
    z := NIL;
    whist := NIL;
    zlast := NIL;
    wlast := NIL;

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
    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);

    Irated := Prated / Vrated / FNphases;
    BaseVolt := Vrated;
    if FNPhases = 3 then 
    begin
        Irated := Irated * sqrt(3);
        BaseVolt := BaseVolt / sqrt(3);
    end;
    BaseCurr := 0.01 * Ppct * Irated;
    Fkv := 1.0 / BaseVolt / sqrt(2.0);
    Fki := BaseCurr * sqrt(2.0);

    if Ffilter <> NIL then
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
            Curr^[i] := -ComplexBuffer^[i];
    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for VCCS Element: %s.'), [Name]), 
                E.Message, _('Inadequate storage allotted for circuit element?'), 335);
    end;
end;

procedure TVCCSObj.UpdateSequenceVoltage;
begin
    if FNPhases = 3 then
        sV1 := (Vterminal^[1] + (ALPHA1 * Vterminal^[2] + ALPHA2 * Vterminal^[3])) / 3.0
    else
        sV1 := Vterminal^[1];
end;

procedure TVCCSObj.GetInjCurrents(Curr: pComplexArray);
var
    i: Integer;
    i1: complex;
begin
    if not Closed[1] then 
    begin
        for i := 1 to Fnphases do 
            Curr^[i] := CZERO;
        Exit;
    end;

    ComputeVterminal;
    UpdateSequenceVoltage;

    // IterminalUpdated := FALSE;
    if ActiveCircuit.Solution.IsDynamicModel then
    begin
        if FrmsMode then
        begin
            i1 := pdegtocomplex (s4 * BaseCurr, cdang (sV1));
            case Fnphases of
                1: 
                    Curr^[1] := i1;
                3: 
                begin
                    Curr^[1] := i1;
                    Curr^[2] := i1 * ALPHA2;
                    Curr^[3] := i1 * ALPHA1;
                end;
            else
                for i := 1 to Fnphases do
                begin
                    Curr^[i] := pdegtocomplex(s4 * BaseCurr, cdang(Vterminal^[i]));
                end;
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

procedure TVCCSObj.MakePosSequence();
begin
    if Fnphases > 1 then
        SetInteger(ord(TProp.Phases), 1);
    inherited;
end;

// support for DYNAMICMODE
// NB: in phasor mode, use load convention for OpenDSS
procedure TVCCSObj.ShutoffInjections; // stop injecting if the terminal opens
var
    i: integer;
begin
    for i:= 1 to FFiltlen do 
    begin
        whist[i] := 0.0;
        wlast[i] := 0.0;
        z[i] := 0.0;
        zlast[i] := 0.0;
    end;
    for i:= 1 to FWinlen do 
        y2[i] := 0.0;

    s1 := 0;
    s2 := 0;
    s3 := 0;
    s4 := 0;
    s5 := 0;
    s6 := 0;
end;

procedure TVCCSObj.InitPhasorStates;
var
    i, k: Integer;
begin
    ComputeIterminal;
    s1 := cabs(Vterminal^[1]) / BaseVolt;
    s4 := cabs(Iterminal^[1]) / BaseCurr;
    s2 := s4;
    s3 := s4;
    s5 := 0;
    s6 := 0;
    sV1 := cmplx(1.0, 0.0);
    vlast := Vterminal^[1] / BaseVolt;

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
    s1 := cabs(Vterminal^[1]) / BaseVolt;
    s3 := cabs(Iterminal^[1]) / BaseCurr;
    s2 := s3;
    s4 := s3;
    s5 := 0;
    s6 := 0;
    sV1 := cmplx(1.0, 0.0);
    vlast := Vterminal^[1] / BaseVolt;

  // initialize the history terms for HW model source convention
    d := 1 / FsampleFreq;
    wd := 2 * Pi * ActiveCircuit.Solution.Frequency * d;
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
    UpdateSequenceVoltage;
    vpu := cabs(sV1) / BaseVolt;
    if vpu > 0.0 then
    begin
        h := ActiveCircuit.Solution.DynaVars.h;
        corrector := ActiveCircuit.Solution.DynaVars.IterationFlag;
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
    if not Closed[1] then 
    begin
        ShutoffInjections;
        Exit;
    end;
    if FrmsMode then
    begin
        IntegratePhasorStates;
        exit;
    end;

    ComputeIterminal;

    t := ActiveCircuit.Solution.DynaVars.t;
    h := ActiveCircuit.Solution.DynaVars.h;
    f := ActiveCircuit.Solution.Frequency;
    corrector := ActiveCircuit.Solution.DynaVars.IterationFlag;
    d := 1 / FSampleFreq;
    nstep := trunc(1e-6 + h / d);
    w := 2 * Pi * f;

    vnow := Vterminal^[1] / BaseVolt;
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

initialization
    ALPHA1 := cmplx(-0.5, 0.5 * sqrt(3.0));  // 1 at 120 degrees
    ALPHA2 := cmplx(-0.5, -ALPHA1.im); // 1 at 240 degrees
    PropInfo := NIL;
end.
