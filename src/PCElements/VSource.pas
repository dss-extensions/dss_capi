unit VSource;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2022, Electric Power Research Institute, Inc.
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
    Spectrum,
    Loadshape;

type
{$SCOPEDENUMS ON}
    TVsourceProp = (
        INVALID = 0,
        bus1 = 1,
        basekv = 2,
        pu = 3,
        angle = 4,
        frequency = 5,
        phases = 6,
        MVAsc3 = 7,
        MVAsc1 = 8,
        x1r1 = 9,
        x0r0 = 10,
        Isc3 = 11,
        Isc1 = 12,
        R1 = 13,
        X1 = 14,
        R0 = 15,
        X0 = 16,
        ScanType = 17,
        Sequence = 18,
        bus2 = 19,
        Z1 = 20,
        Z0 = 21,
        Z2 = 22,
        puZ1 = 23,
        puZ0 = 24,
        puZ2 = 25,
        baseMVA = 26,
        Yearly = 27,
        Daily = 28,
        Duty = 29,
        Model = 30,
        puZideal = 31
    );
{$SCOPEDENUMS OFF}
    TVsource = class(TPCClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TVsourceObj = class(TPCElement)
    PRIVATE
        MVAsc3: Double;
        MVAsc1: Double;
        Isc3: Double;
        Isc1: Double;
        ZSpecType: Integer;
        R1, X1: Double;  // Pos Seq Z
        R2, X2: Double;  // Neg Seq Z
        R0, X0: Double;  // Zero Seq Z
        X1R1: Double;
        X0R0: Double;
        BaseMVA: Double;
        puZ1, puZ0, puZ2: Complex;
        puZideal: Complex;
        ZBase: Double;

        Bus2Defined: Boolean;
        Z1Specified: Boolean;
        puZ1Specified: Boolean;
        puZ0Specified: Boolean;
        puZ2Specified: Boolean;
        Z2Specified: Boolean;
        Z0Specified: Boolean;
        IsQuasiIdeal: LongBool;  // Use puZideal for power flow

        ScanType: Integer;
        SequenceType: Integer;

        ShapeFactor: Complex;
        ShapeIsActual: Boolean;
        procedure GetVterminalForSource;

        procedure CalcDailyMult(Hr: Double);
        procedure CalcDutyMult(Hr: Double);
        procedure CalcYearlyMult(Hr: Double);

        procedure GetInjCurrents(Curr: pComplexArray); 

    PUBLIC
        Z: TCmatrix;  // Base Frequency Series Z matrix
        Zinv: TCMatrix;
        VMag: Double;

        kVBase: Double;
        PerUnit: Double;
        Angle: Double;
        SrcFrequency: Double;

        DailyShapeObj: TLoadShapeObj;  // Daily (24 HR) load shape
        DutyShapeObj: TLoadShapeObj;  // Duty cycle load shape FOR changes typically less than one hour
        YearlyShapeObj: TLoadShapeObj; 

        constructor Create(ParClass: TDSSClass; const SourceName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        function InjCurrents: Integer; OVERRIDE;
        procedure GetCurrents(Curr: pComplexArray); OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
    end;

implementation

uses
    Circuit,
    DSSClassDefs,
    DSSGlobals,
    Dynamics,
    Utilities,
    Sysutils,
    Command,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TVsourceObj;
    TProp = TVsourceProp;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    
    ModelEnum: TDSSEnum;

constructor TVsource.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        ModelEnum := TDSSEnum.Create('VSource: Model', True, 1, 1, ['Thevenin', 'Ideal'], [0, Integer(True)]);
    end;

    inherited Create(dssContext, SOURCE or NON_PCPD_ELEM, 'Vsource');
end;

destructor TVsource.Destroy;
begin
    inherited Destroy;
end;

procedure TVSource.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);
    
    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // enum properties
    PropertyType[ord(TProp.ScanType)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.ScanType)] := ptruint(@obj.ScanType);
    PropertyOffset2[ord(TProp.ScanType)] := PtrInt(DSS.ScanTypeEnum);

    PropertyType[ord(TProp.Sequence)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Sequence)] := ptruint(@obj.Sequencetype);
    PropertyOffset2[ord(TProp.Sequence)] := PtrInt(DSS.SequenceEnum);

    PropertyType[ord(TProp.Model)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.Model)] := ptruint(@obj.IsQuasiIdeal); // LongBool as Integer
    PropertyOffset2[ord(TProp.Model)] := PtrInt(ModelEnum);

    // object properties
    PropertyType[ord(TProp.yearly)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.daily)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.duty)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.yearly)] := ptruint(@obj.YearlyShapeObj);
    PropertyOffset[ord(TProp.daily)] := ptruint(@obj.DailyShapeObj);
    PropertyOffset[ord(TProp.duty)] := ptruint(@obj.DutyShapeObj);

    PropertyOffset2[ord(TProp.yearly)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.daily)] := ptruint(DSS.LoadShapeClass);
    PropertyOffset2[ord(TProp.duty)] := ptruint(DSS.LoadShapeClass);

    // integer
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNphases);

    // complex properties
    PropertyType[ord(TProp.puZ0)] := TPropertyType.ComplexProperty;
    PropertyType[ord(TProp.puZ1)] := TPropertyType.ComplexProperty;
    PropertyType[ord(TProp.puZ2)] := TPropertyType.ComplexProperty;
    PropertyType[ord(TProp.puZideal)] := TPropertyType.ComplexProperty;
    PropertyOffset[ord(TProp.puZ0)] := ptruint(@obj.puZ0);
    PropertyOffset[ord(TProp.puZ1)] := ptruint(@obj.puZ1);
    PropertyOffset[ord(TProp.puZ2)] := ptruint(@obj.puZ2);
    PropertyOffset[ord(TProp.puZideal)] := ptruint(@obj.puZideal);

    // "complex parts" properties
    PropertyType[ord(TProp.Z0)] := TPropertyType.ComplexPartsProperty;
    PropertyType[ord(TProp.Z1)] := TPropertyType.ComplexPartsProperty;
    PropertyType[ord(TProp.Z2)] := TPropertyType.ComplexPartsProperty;
    PropertyOffset[ord(TProp.Z0)] := ptruint(@obj.R0); PropertyOffset2[ord(TProp.Z0)] := ptruint(@obj.X0);
    PropertyOffset[ord(TProp.Z1)] := ptruint(@obj.R1); PropertyOffset2[ord(TProp.Z1)] := ptruint(@obj.X1);
    PropertyOffset[ord(TProp.Z2)] := ptruint(@obj.R2); PropertyOffset2[ord(TProp.Z2)] := ptruint(@obj.X2);
    
    // double properties
    PropertyOffset[ord(TProp.basekv)] := ptruint(@obj.kVBase);
    PropertyOffset[ord(TProp.pu)] := ptruint(@obj.PerUnit);
    PropertyOffset[ord(TProp.angle)] := ptruint(@obj.Angle);
    PropertyOffset[ord(TProp.frequency)] := ptruint(@obj.SrcFrequency);
    PropertyOffset[ord(TProp.MVAsc3)] := ptruint(@obj.MVAsc3);
    PropertyOffset[ord(TProp.MVAsc1)] := ptruint(@obj.MVAsc1);
    PropertyOffset[ord(TProp.x1r1)] := ptruint(@obj.X1R1);
    PropertyOffset[ord(TProp.x0r0)] := ptruint(@obj.X0R0);
    PropertyOffset[ord(TProp.Isc3)] := ptruint(@obj.Isc3);
    PropertyOffset[ord(TProp.Isc1)] := ptruint(@obj.Isc1);

    PropertyOffset[ord(TProp.R1)] := ptruint(@obj.R1);
    PropertyOffset[ord(TProp.X1)] := ptruint(@obj.X1);
    PropertyOffset[ord(TProp.R0)] := ptruint(@obj.R0);
    PropertyOffset[ord(TProp.X0)] := ptruint(@obj.X0);
    PropertyFlags[ord(TProp.R1)] := [TPropertyFlag.Redundant];
    PropertyFlags[ord(TProp.X1)] := [TPropertyFlag.Redundant];
    PropertyFlags[ord(TProp.R0)] := [TPropertyFlag.Redundant];
    PropertyFlags[ord(TProp.X0)] := [TPropertyFlag.Redundant];

    PropertyOffset[ord(TProp.baseMVA)] := ptruint(@obj.BaseMVA);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TVsource.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TVsourceObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
var
    S, S2: String;
    i, dotpos: Integer;
begin
    case Idx of
        ord(TProp.bus1):
        begin
            // Special handling for Bus 1
            // Set Bus2 = Bus1.0.0.0
            if not Bus2Defined then // Default Bus2 to zero node of Bus1. (Grounded-Y connection)
            begin
                // Strip node designations from S
                S := GetBus(1);
                dotpos := Pos('.', S);
                if dotpos > 0 then
                    S2 := Copy(S, 1, dotpos - 1)
                else
                    S2 := Copy(S, 1, Length(S));  // copy up to Dot
                for i := 1 to Fnphases do
                    S2 := S2 + '.0';   // append series of ".0"'s

                SetBus(2, S2);    // default setting for Bus2
            end;
        end;
        6:
            NConds := Fnphases;  // Force Reallocation of terminal info
        13:
            R2 := R1;
        14:
            X2 := X1;
        20:
        begin
            Z1Specified := TRUE;
            // default values for Z2, Z0
            if not Z2Specified then
            begin
                R2 := R1;
                X2 := X1;
            end;
            if not Z0Specified then
            begin
                R0 := R1;
                X0 := X1;
            end;
        end;
        21:
            Z0Specified := TRUE;
        22:
            Z2Specified := TRUE;
        23:
        begin
            puZ1Specified := TRUE;
                // default values for Z2, Z0
            if not puZ2Specified then
            begin
                puZ2 := puZ1;
            end;
            if not puZ0Specified then
            begin
                puZ0 := puZ1;
            end;
        end;
        24:
            puZ0Specified := TRUE;
        25:
            puZ2Specified := TRUE;
        // Set shape objects;  returns nil if not valid
        // Sets the kW and kvar properties to match the peak kW demand from the Loadshape
        28:
        begin
            // If Yearly load shape is not yet defined, make it the same as Daily
            if YearlyShapeObj = NIL then
                YearlyShapeObj := DailyShapeObj;
        end;
    end;

    // Set the Z spec type switch depending on which was specified.
    case Idx of
        7, 8:
            ZSpecType := 1;  // MVAsc
        11, 12:
            ZSpecType := 2;  // Isc
        13 .. 16:
            ZSpecType := 3; // Specified in Ohms
        19:
            Bus2Defined := TRUE;
        20..25:
            Zspectype := 3;
    end;

    case Idx of
        2:
            ZBase := SQR(kvBase) / BaseMVA;
        23:
        begin
            Z1Specified := TRUE;
            puZ1Specified := TRUE;
        end;
        24:
            puZ0Specified := TRUE;
        25:
            puZ2Specified := TRUE;
        26:
            ZBase := SQR(kvBase) / BaseMVA;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TVsource.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        RecalcElementData;
        YPrimInvalid := TRUE;
        Exclude(Flags, Flg.EditionActive);
    end;
    Result := True;
end;

procedure TVsourceObj.MakeLike(OtherPtr: Pointer);
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
    VMag := Other.Vmag;
    kVBase := Other.kVBase;
    BaseMVA := Other.BaseMVA;
    PerUnit := Other.PerUnit;
    Angle := Other.Angle;
    MVAsc3 := Other.MVAsc3;
    MVAsc1 := Other.MVAsc1;

    Scantype := Other.Scantype;
    Sequencetype := Other.Sequencetype;
    SrcFrequency := Other.SrcFrequency;

    ZSpecType := Other.ZSpecType;
    R1 := Other.R1;
    X1 := Other.X1;
    R2 := Other.R2;
    X2 := Other.X2;
    R0 := Other.R0;
    X0 := Other.X0;
    X1R1 := Other.X1R1;
    X0R0 := Other.X0R0;
    BaseMVA := Other.BaseMVA;
    puZ1 := Other.puZ1;
    puZ0 := Other.puZ0;
    puZ2 := Other.puZ2;
    ZBase := Other.ZBase;
    Bus2Defined := Other.Bus2Defined;
    Z1Specified := Other.Z1Specified;
    Z2Specified := Other.Z2Specified;
    Z0Specified := Other.Z0Specified;
    puZ0Specified := Other.puZ0Specified;
    puZ1Specified := Other.puZ1Specified;
    puZ2Specified := Other.puZ2Specified;
    IsQuasiIdeal := Other.IsQuasiIdeal;
    puZideal := Other.puZideal;

    // Loadshape stuff
    ShapeIsActual := Other.ShapeIsActual;
    DailyShapeObj := Other.DailyShapeObj;
    DutyShapeObj := Other.DutyShapeObj;
    YearlyShapeObj := Other.YearlyShapeObj;
end;

constructor TVsourceObj.Create(ParClass: TDSSClass; const SourceName: String);
begin
    inherited create(ParClass);
    Name := LowerCase(SourceName);
    DSSObjType := ParClass.DSSClassType; //SOURCE + NON_PCPD_ELEM;  // Don't want this in PC Element List

    FNphases := 3;
    Fnconds := 3;
    Nterms := 2;   // Now a 2-terminal device
    Z := NIL;
    Zinv := NIL;
    MVAsc3 := 2000.0;
    MVAsc1 := 2100.0;
    ZSpecType := 1; // default to MVAsc

    R1 := 1.65;
    X1 := 6.6;
    R2 := R1;
    X2 := X1;
    R0 := 1.9;
    X0 := 5.7;
    Isc3 := 10000.0;
    Isc1 := 10540.0;
    X1R1 := 4.0;
    X0R0 := 3.0;
    PerUnit := 1.0;  // per unit voltage, not impedance
    kVBase := 115.0;
    BaseMVA := 100.0;
    ZBase := SQR(kvBase) / BaseMVA;

    SrcFrequency := BaseFrequency;
    Angle := 0.0;
    Scantype := 1;
    SequenceType := 1;

    Bus2Defined := FALSE;
    Z1Specified := FALSE;
    Z2Specified := FALSE;
    Z0Specified := FALSE;
    puZ0Specified := FALSE;
    puZ2Specified := FALSE;
    puZ1Specified := FALSE;
    IsQuasiIdeal := FALSE;  // you have to turn this flag ON

    puZideal := Cmplx(1.0e-6, 0.001);  // default ideal source pu impedance

    SpectrumObj := DSS.SpectrumClass.DefaultVSource;

    ShapeIsActual := FALSE;
    YearlyShapeObj := NIL;
    DailyShapeObj := NIL;
    DutyShapeObj := NIL;

    Yorder := Fnterms * Fnconds;
    RecalcElementData;
end;

destructor TVsourceObj.Destroy;
begin
    Z.Free;
    Zinv.Free;

    inherited Destroy;
end;

procedure TVsourceObj.RecalcElementData;
var
    Zs, Zm, Z1, Z2, Z0: Complex;
    Value, Value1, Value2: Complex;
    Calpha1, Calpha2: Complex;
    i, j: Integer;

    Factor: Double;

    Rs, Xs, Rm, Xm: Double;
begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;

    // For a Source, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);

    if FNPhases = 1 then
        Factor := 1.0
    else
        Factor := SQRT3;

    Rs := 0.0;
    Rm := 0.0;
    Xs := 0.1;
    Xm := 0.0;

    // Calculate the short circuit impedance and make all other spec types agree
    case ZSpecType of
        1:
        begin  // MVAsc
            X1 := Sqr(KvBase) / MVAsc3 / Sqrt(1.0 + 1.0 / Sqr(X1R1));
            //  Xs   := Sqr(KvBase) / MVAsc1/Sqrt(1.0 + 1.0/Sqr(X0R0)); // Approx
            R1 := X1 / X1R1;
            R2 := R1;  // default Z2 = Z1
            X2 := X1;
            Isc3 := MVAsc3 * 1000.0 / (SQRT3 * kVBase);
            Isc1 := MVAsc1 * 1000.0 / (Factor * kVBase);

            //  Compute R0, X0
            R0 := QuadSolver((1.0 + SQR(X0R0)), (4.0 * (R1 + X1 * X0R0)), (4.0 * (R1 * R1 + X1 * X1) - SQR(3.0 * kVBase * 1000.0 / Factor / Isc1)));
            X0 := R0 * X0R0;

            // for Z matrix
            Xs := (2.0 * X1 + X0) / 3.0;
            Rs := (2.0 * R1 + R0) / 3.0;

            Rm := (R0 - R1) / 3.0;
            Xm := (X0 - X1) / 3.0;
        end;

        2:
        begin  // Isc
            MVAsc3 := SQRT3 * kVBase * Isc3 / 1000.0;
            MVAsc1 := Factor * kVBase * Isc1 / 1000.0;
            X1 := Sqr(KvBase) / MVAsc3 / Sqrt(1.0 + 1.0 / Sqr(X1R1));
            R1 := X1 / X1R1;
            R2 := R1;  // default Z2 = Z1
            X2 := X1;
            //  Compute R0, X0
            R0 := QuadSolver((1.0 + SQR(X0R0)), (4.0 * (R1 + X1 * X0R0)), (4.0 * (R1 * R1 + X1 * X1) - SQR(3.0 * kVBase * 1000.0 / Factor / Isc1)));
            X0 := R0 * X0R0;

            // for Z matrix
            Xs := (2.0 * X1 + X0) / 3.0;
            Rs := (2.0 * R1 + R0) / 3.0;

            Rm := (R0 - R1) / 3.0;
            Xm := (X0 - X1) / 3.0;
        end;

        3:
        begin  // Z1, Z2, Z0    Specified
            // Compute Z1, Z2, Z0 in ohms if Z1 is specified in pu
            if puZ1Specified then
            begin
                R1 := puZ1.re * Zbase;
                X1 := puZ1.im * Zbase;
                R2 := puZ2.re * Zbase;
                X2 := puZ2.im * Zbase;
                R0 := puZ0.re * Zbase;
                X0 := puZ0.im * Zbase;
            end;

            // Compute equivalent Isc3, Isc1, MVAsc3, MVAsc1 values;
            Isc3 := kVBase * 1000.0 / SQRT3 / Cabs(cmplx(R1, X1));

            // compute nominal values for case where Z1=Z2
            // we won't necessarily use it to build Yprim matrix if Z2 <> Z1

            if Fnphases = 1 then
            begin  // Force Z0 and Z2 to be Z1 so Zs is same as Z1
                R0 := R1;
                X0 := X1;
                R2 := R1;
                X2 := X1;
            end;
            Rs := (2.0 * R1 + R0) / 3.0;
            Xs := (2.0 * X1 + X0) / 3.0;

            Isc1 := kVBase * 1000.0 / Factor / Cabs(cmplx(Rs, Xs));
            MVAsc3 := SQRT3 * kVBase * Isc3 / 1000.0;
            MVAsc1 := Factor * kVBase * Isc1 / 1000.0;
            Xm := Xs - X1;

            Rs := (2.0 * R1 + R0) / 3.0;
            Rm := (R0 - R1) / 3.0;
        end;
    end;

    // Update property Value array
    //  Don't change a specified value; only computed ones
    if (R1 = R2) and (X1 = X2) then
    begin
        // Symmetric Matrix Case
        Zs := cmplx(Rs, Xs);
        Zm := cmplx(Rm, Xm);

        for i := 1 to Fnphases do
        begin
            Z.SetElement(i, i, Zs);
            for j := 1 to i - 1 do
            begin
                Z.SetElemsym(i, j, Zm);
            end;
        end;
    end
    else
    begin
        // Asymmetric Matrix case where Z2 <> Z1
        Z1 := Cmplx(R1, X1);
        Z2 := Cmplx(R2, X2);
        Z0 := Cmplx(R0, X0);

        // Diagonals  (all the same)
        Value := Z2 + Z1 + Z0;
        Value := Value / 3.0;
        for i := 1 to Fnphases do
            Z.SetElement(i, i, Value);

        // Off-Diagonals
        if FnPhases = 3 then     // otherwise undefined
        begin
             // There are two possible off-diagonal elements  if Z1 <> Z2
             // Calpha is defined as 1 /_ -120 instead of 1 /_ 120

            Calpha1 := cong(Calpha);           // Change Calpha to agree with textbooks
            Calpha2 := Calpha1 * Calpha1;  // Alpha squared  = 1 /_ 240 = 1/_-120
             //(Z0 + aZ1 + a2 Z2)/3
            Value2 := Calpha2 * Z2 + Calpha1 * Z1 + Z0;
             //(Z0 + a2 Z1 + aZ2)/3
            Value1 := Calpha2 * Z1 + Calpha1 * Z2 + Z0;
             // Apply 1/3 ...
            Value1 := Value1 / 3.0;
            Value2 := Value2 / 3.0;
            with Z do
            begin
                //Lower Triangle
                SetElement(2, 1, Value1);
                SetElement(3, 1, Value2);
                SetElement(3, 2, Value1);
                //Upper Triangle
                SetElement(1, 2, Value2);
                SetElement(1, 3, Value1);
                SetElement(2, 3, Value2);
            end;

        end;

    end;

    // if not specified, compute a value for for puZ1 for display in formedit
    if not (puZ1Specified or puZ0Specified or puZ2Specified) and (Zbase > 0.0) then
    begin
        puZ1.re := R1 / Zbase;
        puZ1.im := X1 / Zbase;
        puZ2.re := R2 / Zbase;
        puZ2.im := X2 / Zbase;
        puZ0.re := R0 / Zbase;
        puZ0.im := X0 / Zbase;
    end;

    case Fnphases of
        1:
            Vmag := kVBase * PerUnit * 1000.0;
    else
        Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
    end;

    Reallocmem(InjCurrent, SizeOf(InjCurrent^[1]) * Yorder);
end;

procedure TVsourceObj.CalcYPrim;
var
    Value: Complex;
    i, j: Integer;
    FreqMultiplier: Double;
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

    with ActiveCircuit.Solution do
    begin
        FYprimFreq := Frequency;
        FreqMultiplier := FYprimFreq / BaseFrequency;

       // **** Quasi Ideal Source for fundamental power flow****

        if ((FreqMultiplier - 1.0) < Epsilon) and IsQuasiIdeal and (not IsHarmonicModel) then
        begin
            // Ideal Source approximation -- impedance matrix is diagonal matrix only
            Zinv.Clear;
            Value := puZIdeal * Zbase;  // convert to ohms
            for i := 1 to Fnphases do
                Zinv.SetElement(i, i, value);
        end

        else

            // **** Normal Thevenin Source ****
            // Put in Series RL Adjusted for frequency -- Use actual values
            for i := 1 to Fnphases do
            begin
                for j := 1 to Fnphases do
                begin
                    Value := Z.GetElement(i, j);
                    Value.im := Value.im * FreqMultiplier; // Modify from base freq
                    Zinv.SetElement(i, j, value);
                end;
            end;
    end;


    Zinv.Invert;  // Invert in place

    if Zinv.InvertError > 0 then
    begin // If error, put in Large series conductance
        DoErrorMsg('TVsourceObj.CalcYPrim', 
            Format(_('Matrix Inversion Error for Vsource "%s"'), [Name]),
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
            //YPrim_series.SetElemsym(i + FNPhases, j, -Value)
            YPrim_series.SetElement(i, j + Fnphases, -Value);
            YPrim_series.SetElement(i + Fnphases, j, -Value);
        end;
    end;

    YPrim.CopyFrom(YPrim_Series);

    // Now Account for Open Conductors
    // For any conductor that is open, zero out row and column
    inherited CalcYPrim;

    YPrimInvalid := FALSE;
end;

procedure TVsourceObj.GetVterminalForSource;
var
    i: Integer;
    Vharm: Complex;
    SrcHarmonic: Double;
begin
    try
        // This formulation will theoretically handle voltage sources of
        // any number of phases assuming they are
        // equally displaced in time.
        with ActiveCircuit.Solution do
        begin
            ShapeIsActual := FALSE;

            // Modify magnitude based on a LOADSHAPE if assigned
            case Mode of
                // Uses same logic as LOAD
                TSolveMode.DAILYMODE:
                begin
                    CalcDailyMult(DynaVars.dblHour); // set Shapefactor.re = Pmult(t) or PerUnit
                end;
                TSolveMode.YEARLYMODE:
                begin
                    CalcYearlyMult(DynaVars.dblHour);
                end;
                TSolveMode.DUTYCYCLE:
                begin
                    CalcDutyMult(DynaVars.dblHour);
                end;
                TSolveMode.DYNAMICMODE:
                begin
                    // This mode allows use of one class of load shape in DYNAMIC mode
                    // Sets Shapefactor.re = pmult(t) or PerUnit value
                    case ActiveCircuit.ActiveLoadShapeClass of
                        USEDAILY:
                            CalcDailyMult(DynaVars.dblHour);
                        USEYEARLY:
                            CalcYearlyMult(DynaVars.dblHour);
                        USEDUTY:
                            CalcDutyMult(DynaVars.dblHour);
                    else
                        ShapeFactor := Cmplx(PerUnit, 0.0); // default to PerUnit + j0 if not known
                    end;
                end;
            end;

            if (Mode = TSolveMode.DAILYMODE) or  // If a loadshape mode simulation
                (Mode = TSolveMode.YEARLYMODE) or
                (Mode = TSolveMode.DUTYCYCLE) or
                (Mode = TSolveMode.DYNAMICMODE) then
            begin  // Loadshape cases
                if ShapeIsActual then
                    Vmag := 1000.0 * ShapeFactor.re  // assumes actual L-N voltage or voltage across source
                else
                    // is pu value
                    case Fnphases of
                        1:
                            Vmag := kVBase * ShapeFactor.re * 1000.0;
                    else
                        Vmag := kVBase * ShapeFactor.re * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
                    end;
            end
            else  // Normal Case
                case Fnphases of
                    1:
                        Vmag := kVBase * PerUnit * 1000.0;
                else
                    Vmag := kVBase * PerUnit * 1000.0 / 2.0 / Sin((180.0 / Fnphases) * PI / 180.0);
                end;

            if IsHarmonicModel then
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
            begin  // non-harmonic modes
                if abs(Frequency - SrcFrequency) > EPSILON2 then
                    Vmag := 0.0;  // Solution Frequency and Source Frequency don't match!
                // NOTE: RE-uses VTerminal space
                for i := 1 to Fnphases do
                begin
                    case Sequencetype of
                        -1:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle + (i - 1) * 360.0 / Fnphases));  // neg seq
                        0:
                            Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle));   // all the same for zero sequence
                    else
                        Vterminal^[i] := pdegtocomplex(Vmag, (360.0 + Angle - (i - 1) * 360.0 / Fnphases));
                    end;
                    VTerminal^[i + Fnphases] := CZERO;    // See comments in GetInjCurrents
                end;

            end;


        end;

    except
        DoSimpleMsg('Error computing Voltages for "%s". Check specification. Aborting.', [FullName], 326);
        if DSS.In_Redirect then
            DSS.Redirect_Abort := TRUE;
    end;
end;

function TVsourceObj.InjCurrents: Integer;
begin
    GetInjCurrents(InjCurrent);
    // This is source injection
    Result := inherited InjCurrents; // Add into system array
end;

procedure TVsourceObj.GetCurrents(Curr: pComplexArray);
var
    i: Integer;
begin
    try
        with    ActiveCircuit.Solution do
        begin
            for i := 1 to Yorder do
                Vterminal^[i] := NodeV^[NodeRef^[i]];

            YPrim.MVMult(Curr, Vterminal);  // Current from Elements in System Y

            GetInjCurrents(ComplexBuffer);  // Get present value of inj currents
            // Add together with yprim currents
            for i := 1 to Yorder do
                Curr^[i] := Curr^[i] - ComplexBuffer^[i];
        end;
    except
        On E: Exception do
            DoErrorMsg(Format(_('GetCurrents for Element: %s.'), [Name]), E.Message,
                _('Inadequate storage allotted for circuit element.'), 327);
    end;
end;

procedure TVsourceObj.GetInjCurrents(Curr: pComplexArray);
begin
    // source injection currents given by this formula:
    // _     _           _         _
    // |Iinj1|           |Vsource  |
    // |     | = [Yprim] |         |
    // |Iinj2|           | 0       |
    // _     _           _         _
    GetVterminalForSource;  // gets voltage vector above
    YPrim.MVMult(Curr, Vterminal);

    ITerminalUpdated := FALSE;
end;

procedure TVsourceObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
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
        FSWriteln(F, Format('BaseFrequency=%.1f', [BaseFrequency]));
        FSWriteln(F, Format('VMag=%.2f', [VMag]));
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

procedure TVSourceObj.CalcDailyMult(Hr: Double);
begin
    if DailyShapeObj <> NIL then
    begin
        ShapeFactor := DailyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DailyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(PerUnit, 0.0); // Default to no daily variation
end;

procedure TVSourceObj.CalcDutyMult(Hr: Double);
begin
    if DutyShapeObj <> NIL then
    begin
        ShapeFactor := DutyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := DutyShapeObj.UseActual;
    end
    else
        CalcDailyMult(Hr);  // Default to Daily Mult IF no duty curve specified
end;

procedure TVSourceObj.CalcYearlyMult(Hr: Double);
begin
    // Yearly curve is assumed to be hourly only
    if YearlyShapeObj <> NIL then
    begin
        ShapeFactor := YearlyShapeObj.GetMultAtHour(Hr);
        ShapeIsActual := YearlyShapeObj.UseActual;
    end
    else
        ShapeFactor := cmplx(PerUnit, 0.0); // Defaults to no variation
end;

procedure TVsourceObj.MakePosSequence();
var
    R1new, X1new, kVnew: Double;
begin
    R1new := R1;
    X1new := X1;
    kVnew := kVbase / SQRT3;
    BeginEdit(True);
    SetInteger(ord(TProp.Phases), 1);
    SetDouble(ord(TProp.basekv), kVnew);
    SetDouble(ord(TProp.R1), R1new);
    SetDouble(ord(TProp.X1), X1new);
    EndEdit(4);
    inherited;
end;

initialization
    PropInfo := NIL;
finalization
    ModelEnum.Free;
end.
