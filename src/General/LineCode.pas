unit LineCode;

{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

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
    TLineCodeProp = (
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

        SymComponentsModel: Boolean;

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
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        
        procedure CalcMatricesFromZ1Z0;
        function GetPropertyValue(Index: Integer): String; OVERRIDE;
        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

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
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer;    

constructor TLineCode.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, DSS_OBJECT, 'LineCode');
end;

destructor TLineCode.Destroy;
begin
    inherited Destroy;
end;

function GetYCScale(obj: TLineCodeObj): Double;
begin
    Result := TwoPi * obj.BaseFrequency * 1.0e-9;
end;

function GetC1C0Scale(obj: TLineCodeObj): Double;
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
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, False);

    // matrix parts
    PropertyType[ord(TProp.rmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.rmatrix)] := ptruint(@obj.Z);
    PropertyFlags[ord(TProp.rmatrix)] := [TPropertyFlag.RealPart];
    
    PropertyType[ord(TProp.xmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.xmatrix)] := ptruint(@obj.Z);
    PropertyFlags[ord(TProp.xmatrix)] := [TPropertyFlag.ImagPart];

    PropertyType[ord(TProp.cmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.cmatrix)] := ptruint(@obj.YC);
    PropertyOffset2[ord(TProp.cmatrix)] := ptruint(@GetYCScale);
    PropertyFlags[ord(TProp.cmatrix)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.ImagPart];

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
    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.nphases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.neutral)] := ptruint(@obj.FNeutralConductor);
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);
    PropertyOffset[ord(TProp.nphases)] := ptruint(@obj.FNPhases);

    // doubles
    PropertyOffset[ord(TProp.baseFreq)] := ptruint(@obj.BaseFrequency);
    PropertyOffset[ord(TProp.normamps)] := ptruint(@obj.NormAmps);
    PropertyOffset[ord(TProp.emergamps)] := ptruint(@obj.EmergAmps);
    PropertyOffset[ord(TProp.faultrate)] := ptruint(@obj.FaultRate);
    PropertyOffset[ord(TProp.pctperm)] := ptruint(@obj.PctPerm);
    PropertyOffset[ord(TProp.repair)] := ptruint(@obj.HrsToRepair);
    PropertyOffset[ord(TProp.Rg)] := ptruint(@obj.Rg);
    PropertyOffset[ord(TProp.Xg)] := ptruint(@obj.Xg);
    PropertyOffset[ord(TProp.rho)] := ptruint(@obj.rho);

    PropertyOffset[ord(TProp.R1)] := ptruint(@obj.R1);
    PropertyOffset[ord(TProp.X1)] := ptruint(@obj.X1);
    PropertyOffset[ord(TProp.R0)] := ptruint(@obj.R0);
    PropertyOffset[ord(TProp.X0)] := ptruint(@obj.X0);
    
    // adv doubles
    PropertyOffset[ord(TProp.C1)] := ptruint(@obj.C1);
    PropertyOffset[ord(TProp.C0)] := ptruint(@obj.C0);
    PropertyScale[ord(TProp.C1)] := 1.0e-9;
    PropertyScale[ord(TProp.C0)] := 1.0e-9;

    PropertyOffset[ord(TProp.B1)] := ptruint(@obj.C1);
    PropertyOffset2[ord(TProp.B1)] := ptruint(@GetC1C0Scale);
    PropertyFlags[ord(TProp.B1)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.Redundant];

    PropertyOffset[ord(TProp.B0)] := ptruint(@obj.C0);
    PropertyOffset2[ord(TProp.B0)] := ptruint(@GetC1C0Scale);
    PropertyFlags[ord(TProp.B0)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.Redundant];

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

procedure TLineCodeObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
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
        1..6, 23, 24:
            SymComponentsModel := TRUE;
        9..11:
        begin
            Include(Flags, Flg.NeedsRecalc);
            SymComponentsModel := FALSE;
        end;
        25:
            setlength(AmpRatings, NumAmpRatings);
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TLineCode.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    with TObj(ptr) do
    begin
        if SymComponentsModel then
            CalcMatricesFromZ1Z0;
        if Flg.NeedsRecalc in Flags then
        begin
            Exclude(Flags, Flg.NeedsRecalc);
            Zinv.Copyfrom(Z);
            Zinv.Invert;
        end;
        Exclude(Flags, Flg.EditionActive);
    end;
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
    Name := LowerCase(LineCodeName);
    DSSObjType := ParClass.DSSClassType;

    FNPhases := 3;  // Directly set conds and phases
    FNeutralConductor := FNphases;  // initialize to last conductor
    R1 := 0.0580;  //ohms per 1000 ft
    X1 := 0.1206;
    R0 := 0.1784;
    X0 := 0.4047;
    C1 := 3.4e-9;  // nf per 1000ft
    C0 := 1.6e-9;
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
        Z.SetElement(i, i, Zs);
        Yc.SetElement(i, i, Ys);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
            Yc.SetElemsym(i, j, Ym);
        end;
    end;
    Zinv.Copyfrom(Z);
    Zinv.Invert;
end;

procedure TLineCodeObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    k,
    i, j: Integer;
    TempStr: String;
begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin
        FSWriteln(F, Format('~ %s=%d', [PropertyName^[1], FNphases]));
        FSWriteln(F, Format('~ %s=%.5f', [PropertyName^[2], R1]));
        FSWriteln(F, Format('~ %s=%.5f', [PropertyName^[3], X1]));
        FSWriteln(F, Format('~ %s=%.5f', [PropertyName^[4], R0]));
        FSWriteln(F, Format('~ %s=%.5f', [PropertyName^[5], X0]));
        FSWriteln(F, Format('~ %s=%.5f', [PropertyName^[6], C1 * 1.0e9]));
        FSWriteln(F, Format('~ %s=%.5f', [PropertyName^[7], C0 * 1.0e9]));
        FSWriteln(F, Format('~ %s=%s',   [PropertyName^[8], PropertyValue[8]]));
        FSWrite(F, '~ ' + PropertyName^[9] + '=' + '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                FSWrite(F, Format('%.8f ', [Z.GetElement(i, j).re]));
            end;
            FSWrite(F, '|');
        end;
        FSWriteln(F, '"');
        FSWrite(F, '~ ' + PropertyName^[10] + '=' + '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                FSWrite(F, Format('%.8f ', [Z.GetElement(i, j).im]));
            end;
            FSWrite(F, '|');
        end;
        FSWriteln(F, '"');
        FSWrite(F, '~ ' + PropertyName^[11] + '=' + '"');
        for i := 1 to FNPhases do
        begin
            for j := 1 to FNphases do
            begin
                FSWrite(F, Format('%.8f ', [(Yc.GetElement(i, j).im / TwoPi / BaseFrequency * 1.0E9)]));
            end;
            FSWrite(F, '|');
        end;
        FSWriteln(F, '"');


        for i := 12 to 21 do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;

        FSWriteln(F, Format('~ %s=%d', [PropertyName^[22], FNeutralConductor]));
        FSWriteln(F, Format('~ %s=%d', [PropertyName^[25], NumAmpRatings]));
        TempStr := '[';
        for  k := 1 to NumAmpRatings do
            TempStr := TempStr + floattoStrf(AmpRatings[k - 1], ffGeneral, 8, 4) + ',';
        TempStr := TempStr + ']';
        FSWriteln(F, Format('~ %s=%s', [PropertyName^[26]]) + TempStr);

		// TODO: check missing linetype here
    end;
end;

function TLineCodeObj.GetPropertyValue(Index: Integer): String;
begin
    if not SymComponentsModel then
        case Index of
            2, 3, 4, 5, 6, 7, 23, 24:
            begin
                Result := '----';
                Exit
            end;
        end;

    Result := inherited GetPropertyValue(index);
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

    if Fnphases > 1 then
    begin
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

        if (NewZ <> NIL) and (NewYC <> NIL) then
        begin
            NewYC.Invert;  // Back to Y

            Numphases := NewZ.order;

            // Get rid of Z and YC and replace
            Z.Free;
            YC.Free;

            Z := NewZ;
            YC := NewYC;

            FNeutralConductor := 0;

            // Change Property values to reflect Kron reduction for save circuit function
            SetAsNextSeq(1);
            SetAsNextSeq(9);
            SetAsNextSeq(10);
            SetAsNextSeq(11);
        end
        else
        begin
            DoSimpleMsg('Kron Reduction failed: %s. Attempting to eliminate Neutral Conductor %d.', [FullName, FNeutralConductor], 103);
        end;
    end
    else
    begin
        DoSimpleMsg('Cannot perform Kron Reduction on a 1-phase LineCode: %s', [FullName], 103);
    end;
end;

initialization
    PropInfo := NIL;
end.
