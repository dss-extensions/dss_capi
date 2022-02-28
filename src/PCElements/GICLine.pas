unit GICLine;

{
  ----------------------------------------------------------
  Copyright (c) 2011-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

// 6-23-2011 Created from VSource object
//
// Simplified 2-terminal VSource with series impedance for GIC studies.
// For representing induced voltages in lines
//
// Contains blocking capacitor inherent in model.  Set C > 0.0 to activate.
//
// Blocking capacitors may also be added as separate items or the branch may be
// disconnected.
//
// Example:
//    New GICline.Myline  Bus1=MyBus1  Bus2=MyBus2  Volts=1234   R=0.5
//
//    This takes the following defaults:
//      Angle=0
//      X=0
//      C=0
//      Frequency=0.1 Hz
//      Sequence = ZERO sequence
//      ScanType = ZERO sequence

interface

uses
    Classes,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    Spectrum;

type
{$SCOPEDENUMS ON}
    TGICLineProp = (
        INVALID = 0,
        bus1 = 1, 
        bus2 = 2, 
        Volts = 3,
        Angle = 4,
        frequency = 5,
        phases = 6,
        R = 7,
        X = 8,
        C = 9,
        EN = 10,
        EE = 11,
        Lat1 = 12,
        Lon1 = 13,
        Lat2 = 14,
        Lon2 = 15
    );
{$SCOPEDENUMS OFF}

    TGICLine = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TGICLineObj = class(TPCElement)
    PRIVATE
        Angle: Double;
        Volts: Double;
        Vmag: Double;  // Present voltage magnitude
        SrcFrequency: Double;
        R,
        X,
        C,
        ENorth,
        EEast,
        Lat1,
        Lon1,
        Lat2,
        Lon2: Double;

        VN, VE: Double;  // components of vmag

        ScanType: Integer;
        SequenceType: Integer;
        VoltsSpecified: Boolean;

        procedure GetVterminalForSource;
        function Compute_VLine: Double;
    PUBLIC
        Z: TCmatrix;  // Base Frequency Series Z matrix
        Zinv: TCMatrix;


        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray);
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
    end;

implementation

uses
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TGICLineObj;
    TProp = TGICLineProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;    

constructor TGICLine.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, GIC_Line, 'GICLine');
end;

destructor TGICLine.Destroy;
begin
    inherited Destroy;
end;

procedure TGICLine.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // double properties (default type)
    PropertyOffset[ord(TProp.Volts)] := ptruint(@obj.Volts);
    PropertyOffset[ord(TProp.Angle)] := ptruint(@obj.Angle);
    PropertyOffset[ord(TProp.R)] := ptruint(@obj.R);
    PropertyOffset[ord(TProp.X)] := ptruint(@obj.X);
    PropertyOffset[ord(TProp.C)] := ptruint(@obj.C);
    PropertyOffset[ord(TProp.Lat1)] := ptruint(@obj.Lat1);
    PropertyOffset[ord(TProp.Lon1)] := ptruint(@obj.Lon1);
    PropertyOffset[ord(TProp.Lat2)] := ptruint(@obj.Lat2);
    PropertyOffset[ord(TProp.Lon2)] := ptruint(@obj.Lon2);
    PropertyOffset[ord(TProp.frequency)] := ptruint(@obj.SrcFrequency);
    PropertyOffset[ord(TProp.EN)] := ptruint(@obj.ENorth);
    PropertyOffset[ord(TProp.EE)] := ptruint(@obj.EEast);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TGICLine.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TGICLineObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    S, S2: String;
    dotpos: Integer;
begin
    case Idx of
        1:
        // Special handling for Bus 1
        // Set Bus2 = Bus1.0.0.0
        begin
            S := GetBus(1);
            // Strip node designations from S
            dotpos := Pos('.', S);
            if dotpos > 0 then
                S2 := Copy(S, 1, dotpos - 1)
            else
                S2 := Copy(S, 1, Length(S));  // copy up to Dot

            SetBus(2, S2);    // default setting for Bus2  is same as Bus1
        end;
        ord(TProp.phases):
            NConds := Fnphases;  // Force Reallocation of terminal info
        3, 4:
            VoltsSpecified := TRUE;
        10..15:
            VoltsSpecified := FALSE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TGICLine.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TGICLineObj.MakeLike(OtherPtr: Pointer);
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

        if Z <> NIL then
            Z.Free;
        if Zinv <> NIL then
            Zinv.Free;

        Z := TCmatrix.CreateMatrix(Fnphases);
        Zinv := TCMatrix.CreateMatrix(Fnphases);
    end;

    Z.CopyFrom(Other.Z);
    // Zinv.CopyFrom(OtherLine.Zinv);
    R := Other.R;
    X := Other.X;
    C := Other.C;
    Volts := Other.Volts;
    Angle := Other.Angle;

    SrcFrequency := Other.SrcFrequency;
    Scantype := Other.Scantype;
    Sequencetype := Other.Sequencetype;
end;

function TGICLineObj.Compute_VLine: Double;
var
    Phi: Double;
    DeltaLat, DeltaLon: Double;
begin
    Phi := (Lat2 + Lat1) / 2.0 * (pi / 180.0);   // deg to radians
    DeltaLat := Lat2 - Lat1;
    DeltaLon := Lon2 - Lon1;
    VE := (111.133 - 0.56 * cos(2.0 * phi)) * DeltaLat * ENorth;
    VN := (111.5065 - 0.1872 * cos(2.0 * phi)) * Cos(phi) * DeltaLon * EEast;
    Result := VN + VE;
end;

constructor TGICLineObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    FNphases := 3;
    Fnconds := 3;
    Nterms := 2;   // Now a 2-terminal device
    Z := NIL;
    Zinv := NIL;
     {Basefrequency := 60.0;} // set in base class

    R := 1.0;
    X := 0.0;
    C := 0.0;

    ENorth := 1.0;
    EEast := 1.0;
    Lat1 := 33.613499;
    Lon1 := -87.373673;
    Lat2 := 33.547885;
    Lon2 := -86.074605;

    VoltsSpecified := FALSE;

    SrcFrequency := 0.1;  // Typical GIC study frequency
    Angle := 0.0;
    Scantype := 0;
    SequenceType := 0; // default to zero sequence (same voltage induced in all phases)

    SpectrumObj := NIL;  // no default

    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

destructor TGICLineObj.Destroy;
begin
    Z.Free;
    Zinv.Free;

    inherited Destroy;
end;

procedure TGICLineObj.RecalcElementData;
var
    Zs, Zm: Complex;
    i, j: Integer;
begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    {Update property Value array}
     { Don't change a specified value; only computed ones}

    Zs := cmplx(R, X);
    Zm := CZERO;

    for i := 1 to Fnphases do
    begin
        Z.SetElement(i, i, Zs);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
        end;
    end;

    if not VoltsSpecified then
        Volts := Compute_VLine;

    Vmag := Volts;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
end;

procedure TGICLineObj.CalcYPrim;
var
    Value: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
    Xc: Double;
begin
    // Build only YPrim Series
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Series = NIL) {YPrimInvalid} then
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

    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

     { Put in Series RL Adjusted for frequency }
    for i := 1 to Fnphases do
    begin
        for j := 1 to Fnphases do
        begin
            Value := Z.GetElement(i, j);
            Value.im := Value.im * FreqMultiplier;  {Modify from base freq}
            Zinv.SetElement(i, j, value);
        end;
    end;

    if C > 0.0 then // Add 1/wC into diagonals of Zinv
    begin
        Xc := -1.0 / (twopi * FYprimFreq * C * 1.0e-6);
        for i := 1 to Fnphases do
            Zinv.AddElement(i, i, Cmplx(0.0, Xc));
    end;

    Zinv.Invert;  {Invert in place}

    if Zinv.InvertError > 0 then
    begin       {If error, put in Large series conductance}
        DoErrorMsg('TGICLineObj.CalcYPrim', 
            Format(_('Matrix Inversion Error for GICLine "%s"'), [Name]),
            _('Invalid impedance specified. Replaced with small resistance.'), 325);
        Zinv.Clear;
        for i := 1 to Fnphases do
            Zinv.SetElement(i, i, Cmplx(1.0 / EPSILON, 0.0));
    end;

   // YPrim_Series.CopyFrom(Zinv);

    for i := 1 to FNPhases do
    begin
        for j := 1 to FNPhases do
        begin
            Value := Zinv.GetElement(i, j);
            YPrim_series.SetElement(i, j, Value);
            YPrim_series.SetElement(i + FNPhases, j + FNPhases, Value);
            YPrim_series.SetElemsym(i + FNPhases, j, -Value)
        end;
    end;

    YPrim.CopyFrom(YPrim_Series);

     {Now Account for Open Conductors}
     {For any conductor that is open, zero out row and column}
    inherited CalcYPrim;

    YPrimInvalid := FALSE;
end;

procedure TGICLineObj.GetVterminalForSource;
var
    i: Integer;
    Vharm: Complex;
    SrcHarmonic: Double;
begin
    try
        // This formulation will theoretically handle voltage sources of any number of phases assuming they are
        // equally displaced in time.
        Vmag := Volts;

        with ActiveCircuit.Solution do

            if IsHarmonicModel and (SpectrumObj <> NIL) then
            begin
                SrcHarmonic := Frequency / SrcFrequency;
                Vharm := SpectrumObj.GetMult(SrcHarmonic) * Vmag;  // Base voltage for this harmonic
                RotatePhasorDeg(Vharm, SrcHarmonic, Angle);  // Rotate for phase 1 shift
                for i := 1 to Fnphases do
                begin
                    Vterminal^[i] := Vharm;
                    VTerminal^[i + Fnphases] := CZERO;
                    if (i < Fnphases) then
                    begin
                        case ScanType of
                            1:
                                RotatePhasorDeg(Vharm, 1.0, -360.0 / Fnphases); // maintain pos seq
                            0: ;  // Do nothing for Zero Sequence; All the same
                        else
                            RotatePhasorDeg(Vharm, SrcHarmonic, -360.0 / Fnphases); // normal rotation
                        end;
                    end;
                end;
            end
            else
            begin  // non-harmonic modes or no spectrum
                if abs(Frequency - SrcFrequency) > EPSILON2 then
                    Vmag := 0.0;  // Solution Frequency and Source Frequency don't match!
       {NOTE: Re-uses VTerminal space}
                for i := 1 to Fnphases do
                begin
                    case Sequencetype of   // Always 0 for GIC
                        -1:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle + (i - 1) * 360.0 / Fnphases));  // neg seq
                        0:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle));   // all the same for zero sequence
                    else
                        Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
                    end;
                // bottom part of the vector is zero
                    VTerminal^[i + Fnphases] := CZERO;    // See comments in GetInjCurrents
                end;
            end;

    except
        DoSimpleMsg('Error computing Voltages for %s. Check specification. Aborting.', [FullName], 326);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

function TGICLineObj.InjCurrents: Integer;
begin
    GetInjCurrents(InjCurrent);
    // This is source injection
    Result := inherited InjCurrents; // Add into system array
end;

procedure TGICLineObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    try
        with ActiveCircuit.Solution do
        begin
            for  i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];

            YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

            GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
            // Add Together  with yprim currents
            for i := 1 to Yorder do
                Curr^[i] := Curr^[i] - ComplexBuffer^[i];

        end;
    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [FullName]), E.Message,
                _('Inadequate storage allotted for circuit element.'), 327);
    end;
end;

procedure TGICLineObj.GetInjCurrents(Curr: pComplexArray);
begin
   { source injection currents given by this formula:
     _     _           _         _
     |Iinj1|           |GICLine  |
     |     | = [Yprim] |         |
     |Iinj2|           | 0       |
     _     _           _         _
   }

    GetVterminalForSource;  // gets voltage vector above
    YPrim.MVMult(Curr, Vterminal);

    ITerminalUpdated := FALSE;
end;

procedure TGICLineObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
    c: Complex;
begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
        for i := 1 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;

    if Complete then
    begin
        FSWriteln(F);
        FSWriteln(F, Format('BaseFrequency=%.1g', [BaseFrequency]));
        FSWriteln(F, Format('Volts=%.2g', [Volts]));
        FSWriteln(F, Format('VMag=%.2g', [VMag]));
        FSWriteln(F, Format('VE=%.4g', [VE]));
        FSWriteln(F, Format('VN=%.4g', [VN]));
        FSWriteln(F, 'Z Matrix=');
        for i := 1 to Fnphases do
        begin
            for j := 1 to i do
            begin
                c := Z.GetElement(i, j);
                FSWrite(F, Format('%.8g +j %.8g ', [C.re, C.im]));
            end;
            FSWriteln(F);
        end;
    end;
end;

procedure TGICLineObj.MakePosSequence();
var
    Volts_new, Angle_new, R_new, X_new: Double;
begin
    Volts_new := Volts;
    Angle_new := Angle;
    R_new := R;
    X_new := X;
    BeginEdit(True);
    SetInteger(ord(TProp.Phases), 1);
    SetDouble(ord(TProp.Volts), Volts_new);
    SetDouble(ord(TProp.Angle), Angle_new);
    SetDouble(ord(TProp.R), R_new);
    SetDouble(ord(TProp.X), X_new);
    EndEdit(5);

    inherited;
end;

end.