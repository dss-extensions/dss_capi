unit GICsource;

// ----------------------------------------------------------
// Copyright (c) 2008-2018, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

// Developed from Isource and GICLine May 2018

interface

uses
    Classes,
    DSSClass,
    PCClass,
    PCElement,
    ucmatrix,
    UComplex, DSSUcomplex,
    Line;

type
{$SCOPEDENUMS ON}
    TGICsourcePropLegacy = (
        INVALID = 0,
        Volts = 1,
        angle = 2,
        frequency = 3,
        phases = 4,
        EN = 5,
        EE = 6,
        Lat1 = 7,
        Lon1 = 8,
        Lat2 = 9,
        Lon2 = 10
    );
    TGICsourceProp = (
        INVALID = 0,
        Volts = 1,
        Angle = 2,
        Frequency = 3,
        Phases = 4,
        EN = 5,
        EE = 6,
        Lat1 = 7,
        Lon1 = 8,
        Lat2 = 9,
        Lon2 = 10
    );
{$SCOPEDENUMS OFF}
    TGICsource = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TGICSourceObj = class(TPCElement)
    PRIVATE
        FphaseShift: Double;
        Bus2Defined: Boolean;

        // Vmag: Double;
        Angle: Double;
        SrcFrequency: Double;
        pLineElem: TLineObj;  // Pointer to associated Line

        VN, VE: Double;  // components of vmag

        LineClass: Tline;

        procedure GetVterminalForSource;
        function Compute_VLine: Double;
        procedure GetInjCurrents(Curr: pComplexArray);
    PUBLIC

        ENorth,
        EEast,
        Lat1,
        Lon1,
        Lat2,
        Lon2: Double;
        Volts: Double;
        VoltsSpecified: Boolean;

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        function InjCurrents: Integer; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;
    end;

implementation

uses
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Utilities,
    Sysutils,
    Command,
    dynamics,
    DSSHelper,
    DSSObjectHelper,
    TypInfo,
    ArrayDef;

type
    TObj = TGICSourceObj;
    TProp = TGICsourceProp;
    TPropLegacy = TGICsourcePropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TGICsource.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, SOURCE or NON_PCPD_ELEM, 'GICsource');
end;

destructor TGICsource.Destroy;
begin
    inherited Destroy;
end;

procedure TGICsource.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    SpecSetNames := ArrayOfString.Create(
        'Volts, Angle',
        'EN, EE, Lat1, Lon1, Lat2, Lon2'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.Volts), ord(TProp.Angle)),
        TSpecSet.Create(ord(TProp.EN), ord(TProp.EE), ord(TProp.Lat1), ord(TProp.Lon1), ord(TProp.Lat2), ord(TProp.Lon2))
    );

    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // double properties (default type)
    PropertyOffset[ord(TProp.Volts)] := ptruint(@obj.Volts);
    PropertyFlags[ord(TProp.Volts)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet];
    
    PropertyOffset[ord(TProp.angle)] := ptruint(@obj.Angle);
    PropertyFlags[ord(TProp.angle)] := [TPropertyFlag.Units_deg];
    
    PropertyOffset[ord(TProp.frequency)] := ptruint(@obj.SrcFrequency);
    PropertyFlags[ord(TProp.frequency)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Units_Hz];

    PropertyOffset[ord(TProp.EN)] := ptruint(@obj.ENorth);
    PropertyFlags[ord(TProp.EN)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_V_per_km];
    
    PropertyOffset[ord(TProp.EE)] := ptruint(@obj.EEast);
    PropertyFlags[ord(TProp.EE)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_V_per_km];
    
    PropertyOffset[ord(TProp.Lat1)] := ptruint(@obj.Lat1);
    PropertyFlags[ord(TProp.Lat1)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_deg];
    
    PropertyOffset[ord(TProp.Lon1)] := ptruint(@obj.Lon1);
    PropertyFlags[ord(TProp.Lon1)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_deg];
    
    PropertyOffset[ord(TProp.Lat2)] := ptruint(@obj.Lat2);
    PropertyFlags[ord(TProp.Lat2)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_deg];
    
    PropertyOffset[ord(TProp.Lon2)] := ptruint(@obj.Lon2);
    PropertyFlags[ord(TProp.Lon2)] := [TPropertyFlag.NoDefault, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_deg];

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;


end;

function TGICsource.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TGICsourceObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of
        ord(TProp.Volts),
        ord(TProp.Angle):
            VoltsSpecified := TRUE;
        ord(TProp.Phases):
        begin
            FphaseShift := 0.0;     // Zero Sequence
            NConds := Fnphases;  // Force Reallocation of terminal info
        end;
        ord(TProp.EN),
        ord(TProp.EE),
        ord(TProp.Lat1),
        ord(TProp.Lon1),
        ord(TProp.Lat2),
        ord(TProp.Lon2):
            VoltsSpecified := FALSE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

function TGICsource.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    obj: TObj;
begin
    obj := TObj(ptr);
    obj.RecalcElementData(); // Updates Volts
    obj.YPrimInvalid := TRUE;
    Exclude(obj.Flags, Flg.EditingActive);
    Result := True;
end;

procedure TGICsourceObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr); // set spectrum,  base frequency

    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        FNphases := Other.Fnphases;
        NConds := Fnphases;  // Forces reallocation of terminal stuff

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

    Volts := Other.Volts;
    Angle := Other.Angle;
    SrcFrequency := Other.SrcFrequency;
    pLineElem := Other.pLineElem;

    ENorth := Other.ENorth;
    EEast := Other.EEast;
    Lat1 := Other.Lat1;
    Lon1 := Other.Lon1;
    Lat2 := Other.Lat2;
    Lon2 := Other.Lon2;

    Bus2Defined := Other.Bus2Defined;

    SpectrumObj := NIL; // Spectrum not allowed
end;

constructor TGICSourceObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := AnsiLowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; // SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List
    LineClass := DSS.DSSClassList.Get(DSS.ClassNames.Find('Line'));
    pLineElem := LineClass.Find(Name); // GICsource name must be same as associated Line
    FNphases := 3;
    Fnconds := 3;
    Nterms := 2;   // 4/27/2018 made a 2-terminal I source

    Volts := 0.0;
    Angle := 0.0;

    ENorth := 1.0;
    EEast := 1.0;
    Lat1 := 33.613499;
    Lon1 := -87.373673;
    Lat2 := 33.547885;
    Lon2 := -86.074605;

    VoltsSpecified := FALSE;
    SrcFrequency := 0.1;   // this is the GIC source
    FphaseShift := 0.0;    // always zero sequence
    Bus2Defined := FALSE;

    Yorder := Fnterms * Fnconds;
    // Don't do This here RecalcElementData;

    SpectrumObj := NIL; // Spectrum not allowed
end;

destructor TGICSourceObj.Destroy;
begin
    inherited Destroy;
end;

function TGICSourceObj.Compute_VLine: Double;
var
    Phi: Double;
    DeltaLat, DeltaLon: Double;
begin
    Phi := (Lat2 + Lat1) / 2.0 * (pi / 180.0);   // deg to radians
    DeltaLat := Lat1 - Lat2; // switched 11-20 to get pos GIC for pos ENorth
    DeltaLon := Lon1 - Lon2;
    VE := (111.133 - 0.56 * cos(2.0 * phi)) * DeltaLat * ENorth;
    VN := (111.5065 - 0.1872 * cos(2.0 * phi)) * Cos(phi) * DeltaLon * EEast;
    Result := VN + VE;
end;

procedure TGICSourceObj.RecalcElementData;
var
    GICBus: String;
    LineBus2: String;
begin
    if pLineElem = NIL then
    begin
        pLineElem := LineClass.Find(Name);
        if pLineElem = NIL then
        begin
            DoSimpleMsg('Line Object %s associated with %s not found. Make sure you define it first.', [Name, FullName], 333);
        end;
    end
    else
    begin
        LineBus2 := pLineElem.GetBus(2);
        // If LineBus2 already begins with GIC, Don't insert the GIC Bus
        if CompareTextShortest('GIC_', LineBus2) <> 0 then
        begin
             // Define buses -- inserting a new bus GIC_{Name}
            GICBus := 'GIC_' + Name;
            SetBus(1, GICBus);
            SetBus(2, LineBus2);
             // Redefine the bus2 spec for LineElem
            pLineElem.ParsePropertyValue(ord(TLineProp.Bus2), GICBus, []);
        end;

        Bus2Defined := TRUE;
        if not VoltsSpecified then
            Volts := Compute_VLine;
    end;

    Reallocmem(InjCurrent, SizeOf(InjCurrent[1]) * Yorder);
end;

procedure TGICSourceObj.CalcYPrim;
var
    // Rs, Rm, Rzero: Double;
    i: Integer;
    Value: Complex;
    NegValue: Complex;
begin
    // Build only YPrim Series
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Series = NIL) then // YPrimInvalid
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

    // Assume 0.0001 ohms resistance for GIC Source
    Value := 10000.0;
    NegValue := -Value;
    for i := 1 to Fnphases do
    begin
        YPrim_Series[i, i] := Value;
        YPrim_Series[i + Fnphases, i + Fnphases] := Value;
        YPrim_Series[i, i + Fnphases] := NegValue;
        YPrim_Series[i + Fnphases, i] := NegValue;
    end;

    YPrim.Copyfrom(Yprim_Series);      // Initialize YPrim for series impedances
    // Now Account for Open Conductors
    // For any conductor that is open, zero out row and column
    inherited CalcYPrim;

    YPrimInvalid := FALSE;
end;

procedure TGICSourceObj.GetVterminalForSource;
var
    Vmag: Double;
    i: Integer;
begin
    try
        // If the solution frequency not 0.1 Hz, source is shorted.
        if abs(ActiveCircuit.Solution.Frequency - SrcFrequency) < EPSILON2 then
            Vmag := Volts
        else
            Vmag := 0.0;
        for i := 1 to Fnphases do
        begin
            Vterminal[i] := pdegtocomplex(Vmag, (Angle));   // all the same for zero sequence
                // bottom part of the vector is zero
            VTerminal[i + Fnphases] := 0;    // See comments in GetInjCurrents
        end;

    except
        DoSimpleMsg('Error computing current for %s. Check specification. Aborting.', [FullName], 334);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

function TGICSourceObj.InjCurrents: Integer;
// Sum Currents directly into solution array
begin
    GetInjCurrents(InjCurrent);
    Result := inherited Injcurrents;  // Adds into system array
end;

procedure TGICSourceObj.GetCurrents(Curr: pComplexArray);
// Total currents into a device
var
    i: Integer;
begin
    try
        for i := 1 to Yorder do
            Vterminal[i] := ActiveCircuit.Solution.NodeV[NodeRef[i]];

        YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

        GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
        // Add Together  with yprim currents
        for i := 1 to Yorder do
            Curr[i] := Curr[i] - ComplexBuffer[i];
    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [FullName]), E.Message,
                _('Inadequate storage allotted for circuit element?'), 335);
    end;
end;

procedure TGICSourceObj.GetInjCurrents(Curr: pComplexArray);
// source injection currents given by this formula:
//  _     _           _         _
//  |Iinj1|           |GICLineVolts  |
//  |     | = [Yprim] |         |
//  |Iinj2|           | 0       |
//  _     _           _         _
begin
    GetVterminalForSource;    // only at 0.1 Hz
    YPrim.MVMult(Curr, Vterminal);

    ITerminalUPdated := FALSE;
end;

procedure TGICSourceObj.MakePosSequence();
begin
    if Fnphases > 1 then
        SetInteger(ord(TProp.Phases), 1, []);
    inherited;
end;

end.
