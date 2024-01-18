unit LineCode;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

// The Linecode object is a general DSS object used by all circuits
// as a reference for obtaining line impedances.
//
// The values are set by the normal New and Edit procedures for any DSS object.
//
// The values are retrieved by setting the Code Property in the LineCode Class.
// This sets the active Linecode object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.

uses
    Classes,
    Command,
    DSSClass,
    DSSObject,
    UcMatrix,
    Arraydef;

type
{$SCOPEDENUMS ON}
    TLineCodePropLegacy = (
        INVALID = 0,
        nphases = 1,
        r1 = 2,
        x1 = 3,
        r0 = 4,
        x0 = 5,
        C1 = 6,
        C0 = 7,
        units = 8,
        rmatrix = 9,
        xmatrix = 10,
        cmatrix = 11,
        baseFreq = 12,
        normamps = 13,
        emergamps = 14,
        faultrate = 15,
        pctperm = 16,
        repair = 17,
        Kron = 18,
        Rg = 19,
        Xg = 20,
        rho = 21,
        neutral = 22,
        B1 = 23,
        B0 = 24,
        Seasons = 25,
        Ratings = 26,
        LineType = 27
    );
    TLineCodeProp = (
        INVALID = 0,
        NPhases = 1,
        R1 = 2,
        X1 = 3,
        R0 = 4,
        X0 = 5,
        C1 = 6,
        C0 = 7,
        Units = 8,
        RMatrix = 9,
        XMatrix = 10,
        CMatrix = 11,
        BaseFreq = 12,
        NormAmps = 13,
        EmergAmps = 14,
        FaultRate = 15,
        PctPerm = 16,
        Repair = 17,
        Kron = 18,
        Rg = 19,
        Xg = 20,
        rho = 21,
        Neutral = 22,
        B1 = 23,
        B0 = 24,
        Seasons = 25,
        Ratings = 26,
        LineType = 27
    );
{$SCOPEDENUMS OFF}

    TLineCode = class(TDSSClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLineCodeObj = class(TDSSObject)
    PRIVATE
        FNeutralConductor: Integer;

        procedure DoKronReduction;

    PUBLIC
        NumAmpRatings,
        FNPhases: Integer;

        SymComponentsModel: LongBool;

        Z,         // Base Frequency Series Z matrix
        Zinv,
        YC: TCMatrix;  // Shunt capacitance matrix at Base frequency.

        BaseFrequency: Double;

        R1,
        X1,
        R0,
        X0,
        C1,
        C0,
        NormAmps,
        EmergAmps,
        FaultRate,
        PctPerm,
        HrsToRepair,
        Rg,
        Xg,
        rho: Double;
        AmpRatings: array of Double;
        FLineType: Integer; // Pointer to code for type of line

        Units: Integer; // See LineUnits

        constructor Create(ParClass: TDSSClass; const LineCodeName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        
        procedure CalcMatricesFromZ1Z0;
        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

        procedure Set_NumPhases(Value: Integer);
        property NumPhases: Integer read FNPhases write Set_NumPhases;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    UComplex, DSSUcomplex,
    Utilities,
    LineUnits,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TLineCodeObj;
    TProp = TLineCodeProp;
    TPropLegacy = TLineCodePropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TLineCode.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    inherited Create(dssContext, DSS_OBJECT, 'LineCode');
end;

destructor TLineCode.Destroy;
begin
    inherited Destroy;
end;

function GetYCScale(obj: TLineCodeObj; getter: Boolean): Double;
begin
    Result := TwoPi * obj.BaseFrequency * 1.0e-9;
end;

function GetC1C0Scale(obj: TLineCodeObj; getter: Boolean): Double;
begin
    Result := 1 / (TwoPi * obj.BaseFrequency) * 1.0e-6;
end;

procedure Action_KronReduction(obj: TObj);
begin
    obj.DoKronReduction(); 
end;

procedure TLineCode.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy, False);

    SpecSetNames := ArrayOfString.Create(
        //'Z0, Z1',
        'Z0, Z1, C0, C1',
        'Z0, Z1, B0, B1',
        'ZMatrix, CMatrix'
    );
    SpecSets := TSpecSets.Create(
        //TSpecSet.Create(ord(TProp.r1), ord(TProp.x1), ord(TProp.r0), ord(TProp.x0)),
        TSpecSet.Create(ord(TProp.r1), ord(TProp.x1), ord(TProp.r0), ord(TProp.x0), ord(TProp.C1), ord(TProp.C0)),
        TSpecSet.Create(ord(TProp.r1), ord(TProp.x1), ord(TProp.r0), ord(TProp.x0), ord(TProp.B1), ord(TProp.B0)),
        TSpecSet.Create(ord(TProp.rmatrix), ord(TProp.xmatrix), ord(TProp.cmatrix))
    );

    // matrix parts
    PropertyType[ord(TProp.rmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.rmatrix)] := ptruint(@obj.Z);
    PropertyOffset3[ord(TProp.rmatrix)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.rmatrix)] := [TPropertyFlag.RealPart, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_ohm_per_length];
    
    PropertyType[ord(TProp.xmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.xmatrix)] := ptruint(@obj.Z);
    PropertyOffset3[ord(TProp.xmatrix)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.xmatrix)] := [TPropertyFlag.ImagPart, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_ohm_per_length];

    PropertyType[ord(TProp.cmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.cmatrix)] := ptruint(@obj.YC);
    PropertyOffset2[ord(TProp.cmatrix)] := ptruint(@GetYCScale);
    PropertyOffset3[ord(TProp.cmatrix)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.cmatrix)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.ImagPart, TPropertyFlag.Units_nF_per_length];

    // boolean properties
    PropertyType[ord(TProp.Kron)] := TPropertyType.BooleanActionProperty;
    PropertyOffset[ord(TProp.Kron)] := ptruint(@Action_KronReduction);

    // enums
    PropertyType[ord(TProp.units)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.units)] := ptruint(@obj.Units);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);

    PropertyType[ord(TProp.linetype)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.linetype)] := ptruint(@obj.FLineType);
    PropertyOffset2[ord(TProp.linetype)] := PtrInt(DSS.LineTypeEnum);

    // double arrays
    PropertyType[ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Ratings)] := ptruint(@obj.AmpRatings);
    PropertyOffset2[ord(TProp.Ratings)] := ptruint(@obj.NumAmpRatings);

    // integers
    PropertyType[ord(TProp.neutral)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.nphases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.neutral)] := ptruint(@obj.FNeutralConductor);
    PropertyOffset[ord(TProp.nphases)] := ptruint(@obj.FNPhases);

    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);
    PropertyFlags[ord(TProp.Seasons)] := [TPropertyFlag.SuppressJSON]; // can be derived trivially from length(Ratings)

    // doubles
    PropertyOffset[ord(TProp.baseFreq)] := ptruint(@obj.BaseFrequency);
    PropertyFlags[ord(TProp.basefreq)] := [TPropertyFlag.DynamicDefault, TPropertyFlag.NonNegative, TPropertyFlag.NonZero, TPropertyFlag.Units_Hz];

    PropertyOffset[ord(TProp.normamps)] := ptruint(@obj.NormAmps);
    PropertyOffset[ord(TProp.emergamps)] := ptruint(@obj.EmergAmps);
    PropertyOffset[ord(TProp.faultrate)] := ptruint(@obj.FaultRate);
    PropertyOffset[ord(TProp.pctperm)] := ptruint(@obj.PctPerm);
    PropertyOffset[ord(TProp.repair)] := ptruint(@obj.HrsToRepair);

    PropertyOffset[ord(TProp.Rg)] := ptruint(@obj.Rg);
    PropertyFlags[ord(TProp.Rg)] := [TPropertyFlag.Units_ohm_per_length];
    PropertyOffset[ord(TProp.Xg)] := ptruint(@obj.Xg);
    PropertyFlags[ord(TProp.Xg)] := [TPropertyFlag.Units_ohm_per_length];

    PropertyOffset[ord(TProp.rho)] := ptruint(@obj.rho);
    PropertyFlags[ord(TProp.rho)] := [TPropertyFlag.Units_ohmMeter];

    PropertyOffset[ord(TProp.R1)] := ptruint(@obj.R1);
    PropertyOffset[ord(TProp.X1)] := ptruint(@obj.X1);
    PropertyOffset[ord(TProp.R0)] := ptruint(@obj.R0);
    PropertyOffset[ord(TProp.X0)] := ptruint(@obj.X0);
    PropertyFlags[ord(TProp.R1)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_ohm_per_length];
    PropertyFlags[ord(TProp.X1)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_ohm_per_length];
    PropertyFlags[ord(TProp.R0)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.Units_ohm_per_length];
    PropertyFlags[ord(TProp.X0)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.Units_ohm_per_length];
    
    // adv doubles
    PropertyOffset[ord(TProp.C1)] := ptruint(@obj.C1);
    PropertyOffset[ord(TProp.C0)] := ptruint(@obj.C0);
    PropertyScale[ord(TProp.C1)] := 1.0e-9;
    PropertyScale[ord(TProp.C0)] := 1.0e-9;
    PropertyFlags[ord(TProp.C1)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.Units_nF_per_length, TPropertyFlag.RequiredInSpecSet];
    PropertyFlags[ord(TProp.C0)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.Units_nF_per_length];
    
    PropertyOffset[ord(TProp.B1)] := ptruint(@obj.C1);
    PropertyOffset2[ord(TProp.B1)] := ptruint(@GetC1C0Scale);
    PropertyFlags[ord(TProp.B1)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.Redundant, TPropertyFlag.ConditionalValue, TPropertyFlag.RequiredInSpecSet, TPropertyFlag.Units_uS_per_length];
    PropertyRedundantWith[ord(TProp.B1)] := ord(TProp.C1);

    PropertyOffset[ord(TProp.B0)] := ptruint(@obj.C0);
    PropertyOffset2[ord(TProp.B0)] := ptruint(@GetC1C0Scale);
    PropertyFlags[ord(TProp.B0)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.Redundant, TPropertyFlag.ConditionalValue, TPropertyFlag.Units_uS_per_length];
    PropertyRedundantWith[ord(TProp.B0)] := ord(TProp.C0);

    PropertyOffset3[ord(TProp.r1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.x1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.C1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.B1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.r0)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.x0)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.C0)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.B0)] := ptruint(@obj.SymComponentsModel);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TLineCode.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TLineCodeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
begin
    case Idx of
        ord(TProp.nphases):
            if FNphases <> previousIntVal then
            begin
                FNeutralConductor := FNphases;  // Init to last conductor
                // Put some reasonable values in these matrices
                CalcMatricesFromZ1Z0;  // reallocs matrices
            end;
    end;
    case Idx of
        ord(TProp.NPhases),
        ord(TProp.R1),
        ord(TProp.X1),
        ord(TProp.R0),
        ord(TProp.X0),
        ord(TProp.C1),
        // ord(TProp.C0), -- Missing?
        ord(TProp.B1),
        ord(TProp.B0):
        begin
            SymComponentsModel := TRUE;
            if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
            begin
                PrpSequence[ord(TProp.RMatrix)] := 0;
                PrpSequence[ord(TProp.XMatrix)] := 0;
                PrpSequence[ord(TProp.CMatrix)] := 0;
            end;
        end;
        ord(TProp.RMatrix),
        ord(TProp.XMatrix),
        ord(TProp.CMatrix):
        begin
            Include(Flags, Flg.NeedsRecalc);
            SymComponentsModel := FALSE;
            if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
            begin
                PrpSequence[ord(TProp.r1)] := 0;
                PrpSequence[ord(TProp.x1)] := 0;
                PrpSequence[ord(TProp.r0)] := 0;
                PrpSequence[ord(TProp.x0)] := 0;
                PrpSequence[ord(TProp.C1)] := 0;
                PrpSequence[ord(TProp.C0)] := 0;
                PrpSequence[ord(TProp.B1)] := 0;
                PrpSequence[ord(TProp.B0)] := 0;
            end;
        end;
        ord(TProp.Seasons):
            setlength(AmpRatings, NumAmpRatings);
    end;
    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

function TLineCode.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
var
    obj: TObj;
begin
    obj := TObj(ptr);
    if obj.SymComponentsModel then
        obj.CalcMatricesFromZ1Z0();
    if Flg.NeedsRecalc in obj.Flags then
    begin
        Exclude(obj.Flags, Flg.NeedsRecalc);
        obj.Zinv.Copyfrom(obj.Z);
        obj.Zinv.Invert();
    end;
    Exclude(obj.Flags, Flg.EditingActive);
    Result := True;
end;

procedure TLineCodeObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    if FNPhases <> Other.FNphases then
    begin
        FNphases := Other.FNphases;

        if Z <> NIL then
            Z.Free;
        if Zinv <> NIL then
            Zinv.Free;
        if Yc <> NIL then
            Yc.Free;

        Z := TCmatrix.CreateMatrix(FNphases);
        Zinv := TCMatrix.CreateMatrix(FNphases);
        Yc := TCMatrix.CreateMatrix(FNphases);
    end;

    Z.CopyFrom(Other.Z);
    Zinv.CopyFrom(Other.Zinv);
    Yc.CopyFrom(Other.Yc);
    BaseFrequency := Other.BaseFrequency;
    R1 := Other.R1;
    X1 := Other.X1;
    R0 := Other.R0;
    X0 := Other.X0;
    C1 := Other.C1;
    C0 := Other.C0;
    Rg := Other.Rg;
    Xg := Other.Xg;
    rho := Other.rho;
    FNeutralConductor := Other.FNeutralConductor;
    NormAmps := Other.NormAmps;
    EmergAmps := Other.EmergAmps;
    FaultRate := Other.FaultRate;
    PctPerm := Other.PctPerm;
    HrsToRepair := Other.HrsToRepair;
end;

constructor TLineCodeObj.Create(ParClass: TDSSClass; const LineCodeName: String);
begin
    inherited Create(ParClass);
    Name := AnsiLowerCase(LineCodeName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    FNeutralConductor := FNphases;  // initialize to last conductor
    R1 := 0.0580;  //ohms per 1000 ft
    X1 := 0.1206;
    R0 := 0.1784;
    X0 := 0.4047;
    C1 := 3.4e-9;  // nf per 1000ft
    C0 := 1.6e-9;

    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
    begin
        //TODO: should we put everything here, or just the 
        //      required values to disambiguate the spec?
        SetAsNextSeq(ord(TProp.R1));
        SetAsNextSeq(ord(TProp.X1));
        SetAsNextSeq(ord(TProp.R0));
        SetAsNextSeq(ord(TProp.X0));
        SetAsNextSeq(ord(TProp.C1));
        SetAsNextSeq(ord(TProp.C0));
    end;

    Z := NIL;
    Zinv := NIL;
    Yc := NIL;
    Basefrequency := ActiveCircuit.Fundamental;
    Units := UNITS_NONE;  // default to none  (no conversion)
    Normamps := 400.0;
    EmergAmps := 600.0;
    PctPerm := 20.0;
    FaultRate := 0.1;
    FLineType := 1;  // Default to OH Line

    Rg := 0.01805;  // ohms per 1000'
    Xg := 0.155081;
    rho := 100.0;

    SymComponentsModel := TRUE;
    CalcMatricesFromZ1Z0;  // put some reasonable values in

    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;
end;

destructor TLineCodeObj.Destroy;
begin
    Z.Free;
    Zinv.Free;
    Yc.Free;

    inherited destroy;
end;

procedure TLineCodeObj.Set_NumPhases(Value: Integer);
// Set the number of phases and reallocate phase-sensitive arrays
// Need to preserve values in Z matrices
begin
    if Value > 0 then
    begin
        if FNphases <> Value then
        begin    // If size is no different, we don't need to do anything
            FNPhases := Value;
            FNeutralConductor := FNphases;  // Init to last conductor
            // Put some reasonable values in these matrices
            CalcMatricesFromZ1Z0;  // reallocs matrices
        end;
    end;
end;

procedure TLineCodeObj.CalcMatricesFromZ1Z0;
var
    Zs, Zm, Ys, Ym, Ztemp: Complex;
    i, j: Integer;
    Yc1, Yc0, OneThird: Double;
begin
    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;
    if Yc <> NIL then
        Yc.Free;

    // For a line, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(FNphases);
    Zinv := TCMatrix.CreateMatrix(FNphases);
    Yc := TCMatrix.CreateMatrix(FNphases);

    OneThird := 1.0 / 3.0;  // Do this to get more precision in next few statements

    Ztemp := cmplx(R1, X1) * 2;
    Zs := (Ztemp + Cmplx(R0, X0)) * OneThird;
    Zm := (cmplx(R0, X0) - Cmplx(R1, X1)) * OneThird;

    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := (Cmplx(0.0, Yc1) * 2 + Cmplx(0.0, Yc0)) * OneThird;
    Ym := (cmplx(0.0, Yc0) - Cmplx(0.0, Yc1)) * OneThird;

    for i := 1 to FNphases do
    begin
        Z[i, i] := Zs;
        Yc[i, i] := Ys;
        for j := 1 to i - 1 do
        begin
            Z[i, j] := Zm;
            Z[j, i] := Zm;
            Yc[i, j] := Ym;
            Yc[j, i] := Ym;
        end;
    end;
    Zinv.Copyfrom(Z);
    Zinv.Invert();
end;

procedure TLineCodeObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    k,
    i, j: Integer;
    TempStr: String;
begin
    inherited DumpProperties(F, Complete);

    //TODO: remove magic numbers

    FSWriteln(F, Format('~ %s=%d', [ParentClass.PropertyName[1], FNphases]));
    FSWriteln(F, Format('~ %s=%.5f', [ParentClass.PropertyName[2], R1]));
    FSWriteln(F, Format('~ %s=%.5f', [ParentClass.PropertyName[3], X1]));
    FSWriteln(F, Format('~ %s=%.5f', [ParentClass.PropertyName[4], R0]));
    FSWriteln(F, Format('~ %s=%.5f', [ParentClass.PropertyName[5], X0]));
    FSWriteln(F, Format('~ %s=%.5f', [ParentClass.PropertyName[6], C1 * 1.0e9]));
    FSWriteln(F, Format('~ %s=%.5f', [ParentClass.PropertyName[7], C0 * 1.0e9]));
    FSWriteln(F, Format('~ %s=%s',   [ParentClass.PropertyName[8], PropertyValue[8]]));
    FSWrite(F, '~ ' + ParentClass.PropertyName[9] + '=' + '"');
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            FSWrite(F, Format('%.8f ', [Z[i, j].re]));
        end;
        FSWrite(F, '|');
    end;
    FSWriteln(F, '"');
    FSWrite(F, '~ ' + ParentClass.PropertyName[10] + '=' + '"');
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            FSWrite(F, Format('%.8f ', [Z[i, j].im]));
        end;
        FSWrite(F, '|');
    end;
    FSWriteln(F, '"');
    FSWrite(F, '~ ' + ParentClass.PropertyName[11] + '=' + '"');
    for i := 1 to FNPhases do
    begin
        for j := 1 to FNphases do
        begin
            FSWrite(F, Format('%.8f ', [(Yc[i, j].im / TwoPi / BaseFrequency * 1.0E9)]));
        end;
        FSWrite(F, '|');
    end;
    FSWriteln(F, '"');


    for i := 12 to 21 do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue[i]);
    end;

    FSWriteln(F, Format('~ %s=%d', [ParentClass.PropertyName[22], FNeutralConductor]));
    FSWriteln(F, Format('~ %s=%d', [ParentClass.PropertyName[25], NumAmpRatings]));
    TempStr := '[';
    for  k := 1 to NumAmpRatings do
        TempStr := TempStr + floattoStrf(AmpRatings[k - 1], ffGeneral, 8, 4) + ',';
    TempStr := TempStr + ']';
    FSWriteln(F, Format('~ %s=%s', [ParentClass.PropertyName[26], TempStr]));
    FSWriteln(F, Format('~ %s=%s', [ParentClass.PropertyName[27], PropertyValue[27]]));
end;

procedure TLineCodeObj.DoKronReduction;
var
    NewZ, NewYC: TcMatrix;
begin
    if SymComponentsModel then
        Exit;

    if FneutralConductor = 0 then
        Exit;   // Do Nothing

    NewZ := NIL;
    NewYC := NIL;

    if Fnphases <= 1 then
    begin
        DoSimpleMsg('Cannot perform Kron Reduction on a 1-phase LineCode: %s', [FullName], 103);
        Exit;
    end;

    try
        NewZ := Z.Kron(FNeutralConductor);       // Perform Kron Reductions into temp space
        // Have to invert the Y matrix to eliminate properly
        YC.Invert;  // Vn = 0 not In
        NewYC := YC.Kron(FNeutralConductor);
    except
        On E: Exception do
            DoSimpleMsg('Kron Reduction failed: %s. Attempting to eliminate Neutral Conductor %d.', [FullName, FNeutralConductor], 103);
    end;

    // Reallocate into smaller space   if Kron was successful

    if (NewZ = NIL) or (NewYC = NIL) then
    begin
        DoSimpleMsg('Kron Reduction failed: %s. Attempting to eliminate Neutral Conductor %d.', [FullName, FNeutralConductor], 103);
        Exit;
    end;

    NewYC.Invert();  // Back to Y

    Numphases := NewZ.order;

    // Get rid of Z and YC and replace
    Z.Free;
    YC.Free;

    Z := NewZ;
    YC := NewYC;

    FNeutralConductor := 0;

    // Change Property values to reflect Kron reduction for save circuit function
    SetAsNextSeq(ord(TProp.nphases));
    SetAsNextSeq(ord(TProp.rmatrix));
    SetAsNextSeq(ord(TProp.xmatrix));
    SetAsNextSeq(ord(TProp.cmatrix));
end;

end.
