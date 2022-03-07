unit VSConverter;

{
  ----------------------------------------------------------
  Copyright (c) 2013-2015, University of Pittsburgh
  All rights reserved.
  ----------------------------------------------------------
}
interface

uses
    Classes,
    Command,
    DSSClass,
    PCClass,
    Circuit,
    PCElement,
    UcMatrix,
    UComplex, DSSUcomplex,
    ArrayDef,
    XYCurve;

type
{$SCOPEDENUMS ON}
    TVSConverterProp = (
        INVALID = 0,
        phases = 1,
        Bus1 = 2,
        kVac = 3,
        kVdc = 4,
        kW = 5,
        Ndc = 6,
        Rac = 7,
        Xac = 8,
        m0 = 9,
        d0 = 10,
        Mmin = 11,
        Mmax = 12,
        Iacmax = 13,
        Idcmax = 14,
        Vacref = 15,
        Pacref = 16,
        Qacref = 17,
        Vdcref = 18,
        VscMode = 19
    );
{$SCOPEDENUMS OFF}

    TVSConverter = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
   end;

    TVSConverterObj = class(TPCElement)
    PRIVATE
        FkVac: Double;
        FkVdc: Double;
        FkW: Double;
        Fm: Double;
        Fd: Double;
        FRac: Double;
        FXac: Double;
        FrefVac: Double;
        FrefVdc: Double;
        FrefPac: Double;
        FrefQac: Double;
        FMinM: Double;
        FMaxM: Double;
        FMaxIac: Double;
        FMaxIdc: Double;
        Fmode: Integer;
        Ndc: Integer;
        LastCurrents: pComplexArray; // state memory for GetInjCurrents
    PUBLIC
        constructor Create(ParClass: TDSSClass; const FaultName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        // these three functions make it a PCElement
        function InjCurrents: Integer; OVERRIDE;
        procedure GetInjCurrents(Curr: pComplexArray);
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;
    end;


implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Sysutils,
    MathUtil,
    Utilities,
    StrUtils,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TVSConverterObj;
    TProp = TVSConverterProp;
const
    NumPropsThisClass = Ord(High(TProp));
    VSC_FIXED = 0;
    VSC_PACVAC = 1;
    VSC_PACQAC = 2;
    VSC_VDCVAC = 3;
    VSC_VDCQAC = 4;
var
    PropInfo: Pointer = NIL;    
    ModeEnum: TDSSEnum;

constructor TVSConverter.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ModeEnum := TDSSEnum.Create('VSConverter: Control Mode', True, 1, 4,
            ['Fixed', 'PacVac', 'PacQac', 'VdcVac', 'VdcQac'], 
            [0, 1, 2, 3, 4]);
        ModeEnum.DefaultValue := VSC_FIXED;
    end;
    inherited Create(dssContext, VS_CONVERTER, 'VSConverter');
end;

destructor TVSConverter.Destroy;
begin
    inherited Destroy;
end;

procedure TVSConverter.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // enum properties
    PropertyType[ord(TProp.VscMode)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.VscMode)] := ptruint(@obj.Fmode);
    PropertyOffset2[ord(TProp.VscMode)] := PtrInt(ModeEnum);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;

    // integer properties
    PropertyType[ord(TProp.Ndc)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Ndc)] := ptruint(@obj.Ndc);
    
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // double properties (default type)
    PropertyOffset[ord(TProp.kVac)] := ptruint(@obj.FkVac);
    PropertyOffset[ord(TProp.kVdc)] := ptruint(@obj.FkVdc);
    PropertyOffset[ord(TProp.kW)] := ptruint(@obj.FkW);
    PropertyOffset[ord(TProp.Rac)] := ptruint(@obj.FRac);
    PropertyOffset[ord(TProp.Xac)] := ptruint(@obj.FXac);
    PropertyOffset[ord(TProp.m0)] := ptruint(@obj.Fm);
    PropertyOffset[ord(TProp.d0)] := ptruint(@obj.Fd);
    PropertyOffset[ord(TProp.Mmin)] := ptruint(@obj.FMinM);
    PropertyOffset[ord(TProp.Mmax)] := ptruint(@obj.FMaxM);
    PropertyOffset[ord(TProp.Iacmax)] := ptruint(@obj.FMaxIac);
    PropertyOffset[ord(TProp.Idcmax)] := ptruint(@obj.FMaxIdc);
    PropertyOffset[ord(TProp.Vacref)] := ptruint(@obj.FRefVac);
    PropertyOffset[ord(TProp.Pacref)] := ptruint(@obj.FRefPac);
    PropertyOffset[ord(TProp.Qacref)] := ptruint(@obj.FRefQac);
    PropertyOffset[ord(TProp.Vdcref)] := ptruint(@obj.FRefVdc);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TVSConverter.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TVSConverterObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    S, S2: String;
    i, dotpos: Integer;
begin
    case Idx of
        ord(TProp.phases):
            if Fnphases <> previousIntVal then
            begin
                NConds := Fnphases;
                ActiveCircuit.BusNameRedefined := TRUE;
            end;
        ord(TProp.bus1):
        begin
            S := GetBus(1);
            dotpos := Pos('.', S);
            if dotpos > 0 then
                S2 := Copy(S, 1, dotpos - 1)
            else
                S2 := Copy(S, 1, Length(S));
            for i := 1 to Fnphases do
                S2 := S2 + '.0';
            SetBus(2, S2); // default setting for Bus2=Bus1.0.0.0.0
        end;
    end;
    case Idx of
        1..16:
            YprimInvalid := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

procedure TVSConverterObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        Fnphases := Other.Fnphases;
        FnTerms := Other.FnTerms;
        NConds := Fnphases;
    end;
    Ndc := Other.Ndc;
    Yorder := FnConds * FnTerms;
    YPrimInvalid := TRUE;
    FkVac := Other.FkVac;
    FkVdc := Other.FkVdc;
    FkW := Other.FkW;
    FRac := Other.FRac;
    FXac := Other.FXac;
    Fm := Other.Fm;
    Fd := Other.Fd;
    FMinM := Other.FMinM;
    FMaxM := Other.FMaxM;
    FMaxIac := Other.FMaxIac;
    FMaxIdc := Other.FMaxIdc;
    FRefVac := Other.FRefVac;
    FRefPac := Other.FRefPac;
    FRefQac := Other.FRefQac;
    FRefVdc := Other.FRefVdc;
    Fmode := Other.Fmode;
    BaseFrequency := Other.BaseFrequency;
end;

constructor TVSConverterObj.Create(ParClass: TDSSClass; const FaultName: String);
begin
    inherited Create(ParClass);
    DSSObjType := ParClass.DSSClassType;
    Name := AnsiLowerCase(FaultName);

    LastCurrents := NIL;

    // typically the first 3 "phases" are AC, and the last one is DC
    FNPhases := 4;
    Fnconds := 4;
    Nterms := 2; // two-terminal device, like the voltage source
    Ndc := 1;

    FkVac := 1.0;
    FkVdc := 1.0;
    FkW := 1.0;

    Fmode := VSC_FIXED;
    FRac := EPSILON;
    FXac := 0.0;
    Fm := 0.5;
    Fd := 0.0;
    FrefVac := 0.0;
    FrefPac := 0.0;
    FrefQac := 0.0;
    FrefVdc := 0.0;
    FminM := 0.1;
    FmaxM := 0.9;
    FmaxIac := 2.0;
    FmaxIdc := 2.0;

    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

destructor TVSConverterObj.Destroy;
begin
    Reallocmem(LastCurrents, 0);
    inherited destroy;
end;

procedure TVSConverterObj.RecalcElementData;
var
    i: Integer;
begin
    if (FRac = 0.0) and (FXac = 0.0) then
        FRac := EPSILON;
    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
    Reallocmem(LastCurrents, SizeOf(LastCurrents^[1]) * Yorder);
    for i := 1 to Yorder do
        LastCurrents^[i] := CZERO;
end;

procedure TVSConverterObj.CalcYPrim;
var
    Value, Value2: Complex;
    FreqMultiplier: Double;
    i: Integer;
begin
    // build YPrim_Series non-zero for just the AC phases, and it will be diagonal
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Series = NIL) {YPrimInvalid} then
    begin
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        YPrim_Series := TCmatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;
        Yprim.Clear;
    end;

    // calculate the AC voltage source admittance
    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;
    Value.re := FRac;
    Value.im := FXac * FreqMultiplier;
    Value := cinv(Value);
    Value2 := -Value;

    with YPrim_Series do
    begin
        for i := 1 to (Fnphases - Ndc) do
        begin
            SetElement(i, i, Value);
            SetElement(i + Fnphases, i + Fnphases, Value);
            SetElemSym(i, i + Fnphases, Value2);
        end;
    end;
    YPrim.CopyFrom(YPrim_Series);
    inherited CalcYPrim; // may open some conductors
    YprimInvalid := FALSE;
end;

function TVSConverterObj.InjCurrents: Integer;
begin
    GetInjCurrents(InjCurrent);
    Result := inherited InjCurrents; // Add into system array
end;

procedure TVSConverterObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    try
        with ActiveCircuit.Solution do
        begin
            ComputeVTerminal;
            // add the injection currents from both AC and DC nodes, to the
            // currents from Yprim elements, which should be zero at the DC nodes
            YPrim.MVMult(Curr, Vterminal);
            GetInjCurrents(ComplexBuffer);
            for i := 1 to Yorder do
            begin
                Curr^[i] := Curr^[i] - ComplexBuffer^[i];
                LastCurrents^[i] := Curr^[i];
            end;
        end;
    except
        on E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [Name]), 
                E.Message, _('Inadequate storage allotted for circuit element.'), 327);
    end;
end;

procedure TVSConverterObj.GetInjCurrents(Curr: pComplexArray);
var
    Vmag: Complex;
    Vdc, Sphase, Stotal: Complex;
    Pac, Deg, Idc, Idclim{, Iaclim, Itmag}: Double;
    i, Nac: Integer;
begin
    //  AC Voltage source injection currents given by this formula:
    //  _     _           _         _
    //  |Iinj1|           |Vsource  |
    //  |     | = [Yprim] |         |
    //  |Iinj2|           | 0       |
    //  _     _           _         _
   

    Nac := FNphases - Ndc;
    Idclim := FMaxIdc * Fkw / FkVdc;
    // Iaclim := FMaxIac * Fkw / FkVac / Nac;

  // obtain the terminal control quantities
    ComputeVterminal;
    ITerminalUpdated := FALSE;
    GetTerminalCurrents(ITerminal);

  // do the AC voltage source injection - dependent voltage sources kept in ComplexBuffer
    Vdc := Vterminal^[FNphases];
    if (Vdc.re = 0.0) and (Vdc.im = 0.0) then
        Vdc.re := 1000.0 * FkVdc;
    Vmag := Vdc * (0.353553 * Fm);
    RotatePhasorDeg(Vmag, 1.0, Fd);
    ComplexBuffer^[1] := Vmag;
    Deg := -360.0 / Nac;
    for i := 2 to Nac do
    begin
        RotatePhasorDeg(Vmag, 1.0, Deg);
        ComplexBuffer^[i] := Vmag;
    end;
    ComplexBuffer^[FNPhases] := CZERO;
    YPrim.MVMult(Curr, ComplexBuffer);

  // calculate the converter AC power, exclusive of the losses, using LastCurrents
    Stotal.re := 0.0;
    Stotal.im := 0.0;
    for i := 1 to Nac do
    begin
//    Sphase := ComplexBuffer^[i] * cong(LastCurrents^[i]);
        Sphase := ComplexBuffer^[i] * cong(Iterminal^[i]);
        Stotal := Stotal + Sphase;
    end;
    Pac := Stotal.re;
//  Qac := Stotal.im;
    if (Pac = 0.0) then
        Pac := 1000.0 * FkW;

  // DC current source injection
    Idc := Pac / Cabs(Vdc);
    if Idc > Idclim then
        Idc := Idclim;
    if Idc < -Idclim then
        Idc := -Idclim;

    Curr^[FNphases] := cmplx(Idc, 0.0);
    Curr^[2 * FNphases] := cmplx(-Idc, 0.0);
    ITerminalUpdated := FALSE;
end;

procedure TVSConverterObj.MakePosSequence();
begin
    if FnPhases <> 2 then
    begin
        //TODO: why two edits?
        SetInteger(ord(TProp.Phases), 2);
        SetInteger(ord(TProp.Ndc), 1);
    end;
    inherited;
end;

finalization    ModeEnum.Free;
end.
