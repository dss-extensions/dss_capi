unit Reactor;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

//Basic  Reactor
//
//  Uses same rules as Capacitor and Fault for connections
//
//  Implemented as a two-terminal constant impedance (Power Delivery Element)
//  Defaults to a Shunt Reactor but can be connected as a two-terminal series reactor
//
//  If Parallel=Yes, then the R and X components are treated as being in parallel
//
//  Bus2 connection defaults to 0 node of Bus1 (if Bus2 has the default bus connection
//  at the time Bus1 is defined.  Therefore, if only Bus1 is specified, a shunt Reactor results.
//  If delta connected, Bus2 is set to node zero of Bus1 and nothing is returned in the lower
//  half of YPrim - all zeroes.
//
//  If an ungrounded wye is desired, explicitly set Bus2= and set all nodes the same,
//    e.g. Bus1.4.4.4   (uses 4th node of Bus1 as neutral point)
//        or BusNew.1.1.1  (makes a new bus for the neutral point)
//  You must specify the nodes or you will get a series Reactor!
//
//  A series Reactor is specified simply by setting bus2 and declaring the connection
//  to be Wye.  If the connection is specified as delta, nothing will be connected to Bus2.
//  In fact the number of terminals is set to 1.
//
//  Reactance may be specified as:
//
//     1.  kvar and kv ratings at base frequency.  impedance.  Specify kvar as total for
//         all phases. For 1-phase, kV = Reactor coil kV rating.
//         For 2 or 3-phase, kV is line-line three phase. For more than 3 phases, specify
//         kV as actual coil voltage.
//     2.  Series Resistance, R, and Reactance, X, in ohns at base frequency to be used in each phase.  If specified in this manner,
//         the given value is always used whether wye or delta.  X may be specified as Inductance, LmH, in mH.
//         The Z property may also be used to specify R and X in an array.
//     3.  A R and X  matrices .
//         If conn=wye then 2-terminal through device
//         If conn=delta then 1-terminal.
//         Ohms at base frequency
//         Note that Rmatrix may be in parallel with Xmatrix (set parallel = Yes)
//     4.  As symmetrical component values using Z1, Z2, and Z0 complex array properties.
//         Z2 defaults to Z1, but can be set to a different value.

interface

uses
    Classes,
    Command,
    DSSClass,
    PDClass,
    PDElement,
    UComplex, DSSUcomplex,
    UcMatrix,
    ArrayDef,
    XYCurve;

type
{$SCOPEDENUMS ON}
    TReactorPropLegacy = (
        INVALID = 0,   
        bus1 = 1,
        bus2 = 2,
        phases = 3,
        kvar = 4,
        kv = 5,
        conn = 6,
        Rmatrix = 7,
        Xmatrix = 8,
        Parallel = 9,
        R = 10,
        X = 11,
        Rp = 12,
        Z1 = 13,
        Z2 = 14,
        Z0 = 15,
        Z = 16,
        RCurve = 17,
        LCurve = 18,
        LmH = 19
    );
    TReactorProp = (
        INVALID = 0,   
        Bus1 = 1,
        Bus2 = 2,
        Phases = 3,
        kvar = 4,
        kV = 5,
        Conn = 6,
        RMatrix = 7,
        XMatrix = 8,
        Parallel = 9,
        R = 10,
        X = 11,
        Rp = 12,
        Z1 = 13,
        Z2 = 14,
        Z0 = 15,
        Z = 16,
        RCurve = 17,
        LCurve = 18,
        LmH = 19
    );

    TReactorConnection = TGeneralConnection;
    
{$SCOPEDENUMS OFF}

    TReactor = class(TPDClass)
    PROTECTED
        procedure DefineProperties; override;  // Add Properties of this class to propName
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TReactorObj = class(TPDElement)
    PUBLIC
        //TODO: remove R and X, use Z instead
        R, Rp, Gp,
        X, L,
        kvarrating,
        kvrating: Double;
        Z, Z1, Z2, Z0: Complex;
        Rmatrix, Gmatrix,
        XMatrix, Bmatrix: pDoubleArray;  // If not nil then overrides C
        Connection: TReactorConnection;   // 0 or 1 for wye (default) or delta, respectively
        //TODO: Use enum for ReactorObj.SpecType
        SpecType: Integer;   // 1=kvar, 2=R+jX, 3=R and X matrices, 4=sym components
        IsParallel: LongBool;
        RpSpecified: Boolean;
        Bus2Defined: Boolean;
        Z2Specified: Boolean;
        Z0Specified: Boolean;

        RCurveObj: TXYCurveObj;
        LCurveObj: TXYCurveObj;

        constructor Create(ParClass: TDSSClass; const ReactorName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure RecalcElementData(); OVERRIDE;
        procedure CalcYPrim; OVERRIDE;
        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Mathutil,
    Utilities,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TReactorObj;
    TProp = TReactorProp;
    TPropLegacy = TReactorPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TReactor.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, REACTOR_ELEMENT, 'Reactor');
end;

destructor TReactor.Destroy;
begin
    inherited Destroy;
end;

procedure TReactor.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    SpecSetNames := ArrayOfString.Create(
        'kvar, Rcurve, Lcurve',
        'Z, Rcurve, Lcurve',
        'R, Rcurve, Lcurve',
        'R, X, Rcurve, Lcurve',
        'R, LmH, Rcurve, Lcurve',
        'Z0, Z1, Z2',
        'Rmatrix, Xmatrix'
    );
    // Rp is not added when Rmatrix/Xmatrix are used, but
    // it is still used in `TReactorObj.GetLosses`, so we 
    // have to accept it in any spec set.

    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.kvar), ord(TProp.Rcurve), ord(TProp.Lcurve)),
        TSpecSet.Create(ord(TProp.Z), ord(TProp.Rcurve), ord(TProp.Lcurve)),
        TSpecSet.Create(ord(TProp.R), ord(TProp.X), ord(TProp.Rcurve), ord(TProp.Lcurve)),
        TSpecSet.Create(ord(TProp.R), ord(TProp.LmH), ord(TProp.Rcurve), ord(TProp.Lcurve)),
        TSpecSet.Create(ord(TProp.Z0), ord(TProp.Z1), ord(TProp.Z2)),
        TSpecSet.Create(ord(TProp.Rmatrix), ord(TProp.Xmatrix))
    );

    // real matrix
    PropertyType[ord(TProp.Rmatrix)] := TPropertyType.DoubleSymMatrixProperty;
    PropertyOffset[ord(TProp.Rmatrix)] := ptruint(@obj.Rmatrix);
    PropertyOffset3[ord(TProp.Rmatrix)] := ptruint(@obj.Fnphases);

    PropertyType[ord(TProp.Xmatrix)] := TPropertyType.DoubleSymMatrixProperty;
    PropertyOffset[ord(TProp.Xmatrix)] := ptruint(@obj.Xmatrix);
    PropertyOffset3[ord(TProp.Xmatrix)] := ptruint(@obj.Fnphases);
    PropertyFlags[ord(TProp.Xmatrix)] := [TPropertyFlag.RequiredInSpecSet];

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // enums
    PropertyType[ord(TProp.conn)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.conn)] := ptruint(@obj.Connection);
    PropertyOffset2[ord(TProp.conn)] := PtrInt(DSS.ConnectionEnum);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyFlags[ord(TProp.bus1)] := [TPropertyFlag.Required];

    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // boolean properties
    PropertyType[ord(TProp.Parallel)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.Parallel)] := ptruint(@obj.IsParallel);

    // object properties
    PropertyType[ord(TProp.RCurve)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.LCurve)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.RCurve)] := ptruint(@obj.RCurveObj);
    PropertyOffset[ord(TProp.LCurve)] := ptruint(@obj.LCurveObj);

    PropertyOffset2[ord(TProp.RCurve)] := ptruint(DSS.XYCurveClass);
    PropertyOffset2[ord(TProp.LCurve)] := ptruint(DSS.XYCurveClass);

    // complex properties
    PropertyType[ord(TProp.Z)] := TPropertyType.ComplexProperty;
    PropertyOffset[ord(TProp.Z)] := ptruint(@obj.Z);
    PropertyFlags[ord(TProp.Z)] := [TPropertyFlag.RequiredInSpecSet, TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm];

    PropertyType[ord(TProp.Z0)] := TPropertyType.ComplexProperty;
    PropertyOffset[ord(TProp.Z0)] := ptruint(@obj.Z0);
    PropertyFlags[ord(TProp.Z0)] := [TPropertyFlag.RequiredInSpecSet, TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm];

    PropertyType[ord(TProp.Z1)] := TPropertyType.ComplexProperty;
    PropertyOffset[ord(TProp.Z1)] := ptruint(@obj.Z1);
    PropertyFlags[ord(TProp.Z1)] := [TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm];

    PropertyType[ord(TProp.Z2)] := TPropertyType.ComplexProperty;
    PropertyOffset[ord(TProp.Z2)] := ptruint(@obj.Z2);
    PropertyFlags[ord(TProp.Z2)] := [TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm];

    // double properties (default type)
    PropertyOffset[ord(TProp.kvar)] := ptruint(@obj.kvarRating);
    PropertyFlags[ord(TProp.kvar)] := [TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_kvar];

    PropertyOffset[ord(TProp.kv)] := ptruint(@obj.kvRating);
    PropertyFlags[ord(TProp.kV)] := [TPropertyFlag.Required, TPropertyFlag.Units_kV, TPropertyFlag.NonNegative];

    PropertyOffset[ord(TProp.R)] := ptruint(@obj.R);
    PropertyFlags[ord(TProp.R)] := [TPropertyFlag.Redundant, TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm];
    PropertyRedundantWith[ord(TProp.R)] := ord(TProp.Z);

    PropertyOffset[ord(TProp.X)] := ptruint(@obj.X);
    PropertyFlags[ord(TProp.X)] := [TPropertyFlag.Redundant, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm];
    PropertyRedundantWith[ord(TProp.X)] := ord(TProp.Z);

    PropertyOffset[ord(TProp.Rp)] := ptruint(@obj.Rp);
    PropertyFlags[ord(TProp.Rp)] := [TPropertyFlag.NoDefault, TPropertyFlag.Units_ohm];

    // scaled double
    PropertyOffset[ord(TProp.LmH)] := ptruint(@obj.L);
    PropertyScale[ord(TProp.LmH)] := 1.0e-3;
    PropertyFlags[ord(TProp.LmH)] := [TPropertyFlag.Redundant, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_mH]; //TODO: remove redundant now that we have SpecSets?
    PropertyRedundantWith[ord(TProp.LmH)] := ord(TProp.X);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;

    PropertyFlags[PropertyOffset_PDClass + ord(TPDElementProp.normamps)] := [TPropertyFlag.DynamicDefault, TPropertyFlag.Units_A];
    PropertyFlags[PropertyOffset_PDClass + ord(TPDElementProp.emergamps)] := [TPropertyFlag.DynamicDefault, TPropertyFlag.Units_A];
end;

function TReactor.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TReactorObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    S, S2: String;
    i, dotpos: Integer;
begin
    case Idx of
        ord(TProp.bus1):
        begin
            // Default Bus2 to zero node of Bus1 if not already defined. (Wye Grounded connection)
            if (not Bus2Defined) and (Nterms > 1) then
            begin
                // Strip node designations from S
                S := GetBus(1);
                dotpos := Pos('.', S);
                if dotpos > 0 then
                    S2 := Copy(S, 1, dotpos - 1)
                else
                    S2 := Copy(S, 1, Length(S));  // copy up to Dot
                for i := 1 to Fnphases do
                    S2 := S2 + '.0';

                SetBus(2, S2);
                IsShunt := TRUE;
            end;
            PrpSequence[2] := 0;       // Reset this for save function
        end;
        ord(TProp.bus2):
            if AnsiCompareText(StripExtension(GetBus(1)), StripExtension(GetBus(2))) <> 0 then
            begin
                IsShunt := FALSE;
                Bus2Defined := TRUE;
            end;
        ord(TProp.phases):
            if Fnphases <> previousIntVal then
            begin
                // Force Reallocation of terminal info
                if (Connection = TReactorConnection.Delta) then
                begin
                    if (Fnphases = 1) or (Fnphases = 2) then
                        NConds := Fnphases + 1
                    else
                        NConds := Fnphases;
                end
                else
                    NConds := Fnphases;

                Yorder := Fnterms * Fnconds;
            end
            else
            // Probably don't need to check this, but just to be sure...
            if (Connection = TReactorConnection.Delta) and (NConds <> (Fnphases + 1)) then 
            begin
                NConds := Fnphases + 1;
                Yorder := Fnterms * Fnconds;
            end;
        ord(TProp.kvar):
            SpecType := 1;   // X specified by kvar, kV
        ord(TProp.conn):
            case Connection of
                TReactorConnection.Delta:
                begin
                    Nterms := 1;  // Force reallocation of terminals
                    if (Fnphases = 1) or (Fnphases = 2) then
                        NConds := Fnphases + 1
                    else
                        NConds := Fnphases;
                end;
                TReactorConnection.Wye:
                begin
                    if Fnterms <> 2 then
                    begin
                        Nterms := 2;
                        // Yorder := Fnterms * Fnconds;
                    end;
                    NConds := Fnphases;
                end;
            end;
        ord(TProp.Rmatrix),
        ord(TProp.Xmatrix):
            SpecType := 3;
        ord(TProp.X):
            SpecType := 2;   // X specified directly rather than computed from kvar
        ord(TProp.Rp):
            RpSpecified := TRUE;
        ord(TProp.Z1):
        begin
            SpecType := 4;    // have to set Z1 to get this mode
            if not Z2Specified then
                Z2 := Z1;
            if not Z0Specified then
                Z0 := Z1;
        end;
        ord(TProp.Z2):
            Z2Specified := TRUE;
        ord(TProp.Z0):
            Z0Specified := TRUE;
        ord(TProp.Z):
        begin
            R := Z.re;
            X := Z.im;
            SpecType := 2;
        end;
        ord(TProp.LmH):
        begin
            SpecType := 2;
            X := L * TwoPi * BaseFrequency;
        end
    end;

    //YPrim invalidation on anything that changes impedance values
    case Idx of
        ord(TProp.phases),
        ord(TProp.kvar),
        ord(TProp.kv),
        ord(TProp.conn),
        ord(TProp.Rmatrix),
        ord(TProp.Xmatrix),
        ord(TProp.Parallel),
        ord(TProp.R),
        ord(TProp.X),
        ord(TProp.Rp),
        ord(TProp.Z1),
        ord(TProp.Z2),
        ord(TProp.Z0),
        ord(TProp.Z),
        ord(TProp.LmH):
            YprimInvalid := TRUE;
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TReactorObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);  // Take care of inherited class properties

    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        FNPhases := Other.Fnphases;
        NConds := Fnphases; // force reallocation of terminals and conductors

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;
    end;

    R := Other.R;
    X := Other.X;
    Rp := Other.Rp;

    RpSpecified := Other.RpSpecified;
    IsParallel := Other.IsParallel;

    kvarrating := Other.kvarrating;
    kvrating := Other.kvrating;
    Connection := Other.Connection;
    SpecType := Other.SpecType;

    Z := Other.Z;
    Z1 := Other.Z1;
    Z2 := Other.Z2;
    Z0 := Other.Z0;
    Z2Specified := Other.Z2Specified;
    Z0Specified := Other.Z0Specified;

    RCurveobj := Other.RCurveobj;
    LCurveobj := Other.LCurveobj;

    if Other.Rmatrix = NIL then
        Reallocmem(Rmatrix, 0)
    else
    begin
        Reallocmem(Rmatrix, SizeOf(Rmatrix[1]) * Fnphases * Fnphases);
        for i := 1 to Fnphases * Fnphases do
            Rmatrix[i] := Other.Rmatrix[i];
    end;

    if Other.Xmatrix = NIL then
        Reallocmem(Xmatrix, 0)
    else
    begin
        Reallocmem(Xmatrix, SizeOf(Xmatrix[1]) * Fnphases * Fnphases);
        for i := 1 to Fnphases * Fnphases do
            Xmatrix[i] := Other.Xmatrix[i];
    end;
end;

constructor TReactorObj.Create(ParClass: TDSSClass; const ReactorName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(ReactorName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 2;  // Force allocation of terminals and conductors

    Setbus(2, (GetBus(1) + '.0.0.0'));  // Default to grounded wye

    IsShunt := TRUE;

    Rmatrix := NIL;
    Xmatrix := NIL;
    Gmatrix := NIL;
    Bmatrix := NIL;

    kvarrating := 100.0;
    kvrating := 12.47;
    X := SQR(kvrating) * 1000.0 / kvarrating;
    R := 0.0;
    Rp := 0.0;  // Indicates it has not been set to a proper value
    IsParallel := FALSE;
    RpSpecified := FALSE;
    Bus2Defined := FALSE;
    Z2Specified := FALSE;
    Z0Specified := FALSE;
    Connection := TReactorConnection.Wye;   // 0 or 1 for wye (default) or delta, respectively
    SpecType := 1; // 1=kvar, 2=Cuf, 3=Cmatrix
    NormAmps := kvarRating * SQRT3 / kvrating;
    EmergAmps := NormAmps * 1.35;
    FaultRate := 0.0005;
    PctPerm := 100.0;
    HrsToRepair := 3.0;
    Yorder := Fnterms * Fnconds;

    RCurveObj := NIL;
    LCurveObj := NIL;

    RecalcElementData();
end;

destructor TReactorObj.Destroy;
begin
    ReallocMem(Rmatrix, 0);
    ReallocMem(Xmatrix, 0);
    ReallocMem(Gmatrix, 0);
    ReallocMem(Bmatrix, 0);
    inherited destroy;
end;

procedure TReactorObj.RecalcElementData();
var
    KvarPerPhase, PhasekV: Double;
    i, CheckError: Integer;
begin
    case SpecType of
        1:
        begin // kvar
            kvarPerPhase := kvarRating / Fnphases;
            case Connection of
                TReactorConnection.Delta:
                begin  // Line-to-Line
                    PhasekV := kVRating;
                end;
            else
            begin  //  line-to-neutral
                case Fnphases of
                    2, 3:
                        PhasekV := kVRating / SQRT3;  // Assume three phase system
                else
                    PhasekV := kVRating;
                end;
            end;
            end;
            X := SQR(PhasekV) * 1000.0 / kvarPerPhase;
            L := X / twopi / BaseFrequency;
            // Leave R as specified
            if not normAmpsSpecified then 
                NormAmps := kvarPerPhase / PhasekV;
            if not emergAmpsSpecified then 
                EmergAmps := kvarPerPhase / PhasekV * 1.35;
        end;
        2:
        begin // R + j X
          // Nothing much to do
            L := X / twopi / BaseFrequency;
        end;
        3:
        begin // Matrices

        end;
    end;

    if RpSpecified and (Rp <> 0.0) then
        Gp := 1.0 / Rp
    else
        Gp := 0.0; // default to 0,0 if Rp=0;

    if IsParallel and (SpecType = 3) then
    begin
        ReAllocmem(Gmatrix, SizeOf(Gmatrix[1]) * Fnphases * Fnphases);
        ReAllocmem(Bmatrix, SizeOf(Bmatrix[1]) * Fnphases * Fnphases);

        // Copy Rmatrix to Gmatrix and Invert
        for i := 1 to Fnphases * Fnphases do
            Gmatrix[i] := Rmatrix[i];
// should be Gmatrix         ETKInvert(Rmatrix, Fnphases, CheckError);
        ETKInvert(Gmatrix, Fnphases, CheckError);
        if CheckError > 0 then
        begin
            DoSimpleMsg('Error inverting R Matrix for "%s" - G is zeroed.', [FullName], 232);
            for i := 1 to Fnphases * Fnphases do
                Gmatrix[i] := 0.0;
        end;

        // Copy Xmatrix to Bmatrix and Invert
        for i := 1 to Fnphases * Fnphases do
            Bmatrix[i] := -Xmatrix[i];
        ETKInvert(Bmatrix, Fnphases, CheckError);
        if CheckError > 0 then
        begin
            DoSimpleMsg('Error inverting X Matrix for "%s" - B is zeroed.', [FullName], 233);
            for i := 1 to Fnphases * Fnphases do
                Bmatrix[i] := 0.0;
        end;
    end;
end;

procedure TReactorObj.CalcYPrim;
var
    Value, Value1, Value2: Complex;
    Calpha1, CAlpha2: Complex;
 //  Y0, Y1, Y2 : Complex;
    i, j, idx: Integer;
    FreqMultiplier: Double;
    ZValues: pComplexArray;
    YPrimTemp,
    ZMatrix: TCMatrix;
    RValue, LValue: Double;
begin
    // Normally build only Yprim Shunt, but if there are 2 terminals and
    // Bus1 <> Bus 2
    if (Yprim = NIL) OR (Yprim.order <> Yorder) then // YPrimInvalid
    begin    // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        if Yprim_Series <> NIL then
            Yprim_Series.Free;
        Yprim_Series := TcMatrix.CreateMatrix(Yorder);
        if YPrim <> NIL then
            YPrim.Free;
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear; // zero out YPrim
        YPrim_Shunt.Clear; // zero out YPrim
        Yprim.Clear;
    end;

    if IsShunt then
        YPrimTemp := YPrim_Shunt
    else
        YPrimTemp := Yprim_Series;


    FYprimFreq := ActiveCircuit.Solution.Frequency;
    FreqMultiplier := FYprimFreq / BaseFrequency;

    // If GIC simulation, Resistance Only
    if ActiveCircuit.Solution.Frequency < 0.51 then
    begin    // 0.5 Hz is cutoff
        if X > 0.0 then
            if R <= 0.0 then
                R := X / 50.0;   // Assume X/R = 50
        FYprimFreq := 0.0;        // Set these to 0.0 for GIC model
        FreqMultiplier := 0.0;    // sets reactance part to zero
    end;

    // Now, Put in Yprim matrix

    case SpecType of

        1, 2:
        begin   // Some form of R and X specified
            // Adjust for frequency
            if Assigned(RCurveObj) then
                RValue := R * RCurveObj.GetYValue(FYprimFreq)
            else
                RValue := R;
            if Assigned(LCurveObj) then
                LValue := L * LCurveObj.GetYValue(FYprimFreq)
            else
                LValue := L;

            Value := Cinv(Cmplx(RValue, LValue * Twopi * FYprimFreq));
            // Add in Rp Value if specified
            if RpSpecified then
                Value += Gp;

            Value2 := -Value;
            if Connection = TReactorConnection.Delta then
            begin   // Line-Line
                for i := 1 to Fnphases do
                begin
                    j := i + 1;
                    if j > Fnconds then
                        j := 1;

                    YPrimTemp.AddElement(i, i, Value);
                    YPrimTemp.AddElement(j, j, Value);
                    YPrimTemp.AddElement(i, j, Value2);
                    YPrimTemp.AddElement(j, i, Value2);                
                end;
                // Remainder of the matrix is all zero
            end
            else
            begin // Wye
                for i := 1 to Fnphases do
                begin
                    j := i + Fnphases;
                    YPrimTemp[i, i] := Value;     // Elements are only on the diagonals
                    YPrimTemp[j, j] := Value;
                    YPrimTemp[i, j] := Value2;
                    YPrimTemp[j, i] := Value2;
                end;
            end;

        end;

        3:
        begin    // Z matrix specified
            //Compute Z matrix

            // Put in Parallel R & L 
            if IsParallel then
            begin // Build Z as a Y Matrix
                for i := 1 to Fnphases do
                begin
                    for j := 1 to Fnphases do
                    begin
                        idx := (j - 1) * Fnphases + i;
                        // FreqMultiplier = 0 signifies GIC model where we only need R part
                        if FreqMultiplier > 0.0 then
                            Value := Cmplx(Gmatrix[idx], Bmatrix[idx] / FreqMultiplier)
                        else
                            Value := Gmatrix[idx];
                        YPrimTemp[i, j] := Value;
                        YPrimTemp[i + Fnphases, j + Fnphases] := Value;
                        YPrimTemp[i, j + Fnphases] := -Value;
                        YPrimTemp[j + Fnphases, i] := -Value;
                    end;
                end;

            end
            else
            begin // For Series R and X
                Zmatrix := TcMatrix.CreateMatrix(Fnphases);
                ZValues := Zmatrix.GetValuesArrayPtr(Fnphases);  // So we can stuff array fast
                // Put in Series R & L
                for i := 1 to Fnphases * Fnphases do
                begin
                    // Correct the impedances for frequency
                    ZValues[i] := Cmplx(Rmatrix[i], Xmatrix[i] * FreqMultiplier);
                end;

                ZMatrix.Invert(); // Invert in place - is now Ymatrix
                if ZMatrix.InvertError > 0 then
                begin // If error, put in tiny series conductance
                    DoErrorMsg('TReactorObj.CalcYPrim', 
                        Format(_('Matrix Inversion Error for Reactor "%s"'), [Name]),
                        _('Invalid impedance specified. Replaced with tiny conductance.'), 234);
                    ZMatrix.Clear;
                    for i := 1 to Fnphases do
                        ZMatrix[i, i] := epsilon;
                end;

                for i := 1 to Fnphases do
                begin
                    for j := 1 to Fnphases do
                    begin
                        Value := Zmatrix[i, j];
                        YPrimTemp[i, j] := Value;
                        YPrimTemp[i + Fnphases, j + Fnphases] := Value;
                        YPrimTemp[i, j + Fnphases] := -Value;
                        YPrimTemp[j + Fnphases, i] := -Value;
                    end;
                end;

                Zmatrix.Free;
            end;
        end;

        4:
        begin  // Symmetrical component Z's specified
//
//   parallel doesn't make sense
//              If IsParallel Then
//                ...
            
            // Series R+jX

            Zmatrix := TcMatrix.CreateMatrix(Fnphases);

                // diagonal elements  -- all the same
            if Fnphases = 1 then // assume positive sequence only model
                Value := Z1
            else
                Value := Z2 + Z1 + Z0;

            Value.im := Value.im * FreqMultiplier; // Correct the impedances for frequency
            Value := Value / 3.0;
            for i := 1 to Fnphases do
            begin
                Zmatrix[i, i] := Value;
            end;

            if FnPhases = 3 then     // otherwise undefined
            begin
                // There are two possible off-diagonal elements  if Z1 <> Z2
                // Calpha is defined as 1 /_ -120 instead of 1 /_ 120

                Calpha1 := cong(Calpha);   // Change it to agree with textbooks
                Calpha2 := Calpha1 * Calpha1;  // Alpha squared  = 1 /_ 240 = 1/_-120
                Value2 := Calpha2 * Z2 + Calpha1 * Z1 + Z0;
                Value1 := Calpha2 * Z1 + Calpha1 * Z2 + Z0;

                Value1.im := Value1.im * FreqMultiplier; // Correct the impedances for frequency
                Value2.im := Value2.im * FreqMultiplier; // Correct the impedances for frequency

                Value1 := Value1/  3.0;
                Value2 := Value2/  3.0;

                //Lower Triangle
                Zmatrix[2, 1] := Value1;
                Zmatrix[3, 1] := Value2;
                Zmatrix[3, 2] := Value1;
                //Upper Triangle
                Zmatrix[1, 2] := Value2;
                Zmatrix[1, 3] := Value1;
                Zmatrix[2, 3] := Value2;
            end;

            ZMatrix.Invert();  // Invert in place - is now Ymatrix
            if ZMatrix.InvertError > 0 then
            begin // If error, put in tiny series conductance
                DoErrorMsg('TReactorObj.CalcYPrim', 
                    Format(_('Matrix Inversion Error for Reactor "%s"'), [Name]),
                    _('Invalid impedance specified. Replaced with tiny conductance.'), 234);
                ZMatrix.Clear();
                for i := 1 to Fnphases do
                    ZMatrix[i, i] := epsilon;
            end;

            for i := 1 to Fnphases do
            begin
                for j := 1 to Fnphases do
                begin
                    Value := Zmatrix[i, j];
                    YPrimTemp[i, j] := Value;
                    YPrimTemp[i + Fnphases, j + Fnphases] := Value;
                    YPrimTemp[i, j + Fnphases] := -Value;
                    YPrimTemp[i + Fnphases, j] := -Value;
                end;
            end;

            Zmatrix.Free;

        end;
    //    END;
    end;

    // Set YPrim_Series based on diagonals of YPrim_shunt  so that CalcVoltages doesn't fail
    if IsShunt then
    begin
        if (Nphases = 1) and (not ActiveCircuit.PositiveSequence) then  // assume a neutral or grounding reactor; Leave diagonal in the circuit
            for i := 1 to Yorder do
                Yprim_Series[i, i] := Yprim_Shunt[i, i]
        else
            for i := 1 to Yorder do
                Yprim_Series[i, i] := Yprim_Shunt[i, i] * 1.0e-10;
    end;

    Yprim.Copyfrom(YPrimTemp);
    // Don't Free YPrimTemp - It's just a pointer to an existing complex matrix

    inherited CalcYPrim;

    YprimInvalid := FALSE;
end;

procedure TReactorObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i, j, k: Integer;
begin
    inherited DumpProperties(F, Complete);

    for k := 1 to ParentClass.NumProperties do
    begin
        case k of  // was 'CASE i of' - good example of reason to remove all warnings 
            ord(TProp.RMatrix):
                if Rmatrix <> NIL then
                begin
                    FSWrite(F, ParentClass.PropertyName[k] + '= (');
                    for i := 1 to Fnphases do
                    begin
                        for j := 1 to Fnphases do
                            FSWrite(F, Format('%-.5g ', [RMatrix[(i - 1) * Fnphases + j]]));
                        if i <> Fnphases then
                            FSWrite(F, '|');
                    end;
                    FSWriteln(F, ')');
                end;
            ord(TProp.XMatrix):
                if Xmatrix <> NIL then
                begin
                    FSWrite(F, ParentClass.PropertyName[k] + '= (');
                    for i := 1 to Fnphases do
                    begin
                        for j := 1 to Fnphases do
                            FSWrite(F, Format('%-.5g ', [XMatrix[(i - 1) * Fnphases + j]]));
                        if i <> Fnphases then
                            FSWrite(F, '|');
                    end;
                    FSWriteln(F, ')');
                end;
            ord(TProp.Z1):
                FSWriteln(F, Format('~ Z1=[%-.8g, %-.8g]', [Z1.re, Z1.im]));
            ord(TProp.Z2):
                FSWriteln(F, Format('~ Z2=[%-.8g, %-.8g]', [Z2.re, Z2.im]));
            ord(TProp.Z0):
                FSWriteln(F, Format('~ Z0=[%-.8g, %-.8g]', [Z0.re, Z0.im]));
            ord(TProp.Z):
                FSWriteln(F, Format('~ Z =[%-.8g, %-.8g]', [R, X]));
            ord(TProp.LmH):
                FSWriteln(F, Format('~ LmH=%-.8g', [L * 1000.0]));
        else
            FSWriteln(F, '~ ' + ParentClass.PropertyName[k] + '=' + PropertyValue[k]);
        end;
    end;
end;

procedure TReactorObj.GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex);
var
    i: Integer;
    v: Complex;
begin
    // Only report No Load Losses if Rp defined and Reactor is a shunt device;
    // Else do default behavior.
    if (RpSpecified and IsShunt and (Rp <> 0.0)) then
    begin
        TotalLosses := Losses;  // Side effect: computes Iterminal and Vterminal
        // Compute losses in Rp Branch from voltages across shunt element -- node to ground
        NoLoadLosses := 0;
        for i := 1 to FNphases do
        begin
            v := ActiveCircuit.Solution.NodeV[NodeRef[i]];
            NoLoadLosses += (v.re * v.re + v.im * v.im) / Rp;  // V^2/Rp
        end;

        if ActiveCircuit.PositiveSequence then
            NoLoadLosses := NoLoadLosses * 3.0;
        LoadLosses := TotalLosses - NoLoadLosses;  // Subtract no load losses from total losses
        Exit;
    end;
    inherited; // do the default Cktelement behaviors
end;

procedure TReactorObj.MakePosSequence();
var
    kvarperphase, phasekV, Rs, Rm, R, X: Double;
    changes, i, j: Integer;
begin
    changes := 1;
    BeginEdit(True);
    // If FnPhases>1 Then
    begin
        case SpecType of
            2, // R + j X
            4: // symmetrical components  Z1 specified
                SetInteger(ord(TProp.Phases), 1, []);
            1:
            begin // kvar
                kvarPerPhase := kvarRating / 3.0;  // divide among 3 phases Fnphases;
                if (FnPhases > 1) or (Connection <> TReactorConnection.Wye) then
                    PhasekV := kVRating / SQRT3
                else
                    PhasekV := kVRating;

                SetInteger(ord(TProp.Phases), 1, []);
                SetDouble(ord(TProp.kV), PhasekV, []);
                SetDouble(ord(TProp.kvar), kvarPerPhase, []);
                changes := 3;
                // Leave R as specified
            end;
            3:
                if FnPhases > 1 then
                begin // Matrices
                    // R1
                    Rs := 0.0;   // Avg Self
                    for i := 1 to FnPhases do
                        Rs := Rs + Rmatrix[(i - 1) * Fnphases + i];
                    Rs := Rs / FnPhases;
                    Rm := 0.0;     //Avg mutual
                    for i := 2 to FnPhases do
                        for j := i to FnPhases do
                            Rm := Rm + Rmatrix[(i - 1) * Fnphases + j];
                    Rm := Rm / (FnPhases * (Fnphases - 1.0) / 2.0);
                    R := (Rs - Rm);

                    // X1
                    Rs := 0.0;   // Avg Self
                    for i := 1 to FnPhases do
                        Rs := Rs + Xmatrix[(i - 1) * Fnphases + i];
                    Rs := Rs / FnPhases;
                    Rm := 0.0;     //Avg mutual
                    for i := 2 to FnPhases do
                        for j := i to FnPhases do
                            Rm := Rm + Xmatrix[(i - 1) * Fnphases + j];
                    Rm := Rm / (FnPhases * (Fnphases - 1.0) / 2.0);
                    X := (Rs - Rm);

                    SetInteger(ord(TProp.Phases), 1, []);
                    SetDouble(ord(TProp.R), R, []);
                    SetDouble(ord(TProp.X), X, []);
                    changes := 3;
                end;
        end;
    end;
    EndEdit(changes);
    inherited;
end;

end.
