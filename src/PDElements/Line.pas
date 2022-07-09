unit Line;
{
  ----------------------------------------------------------
  Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
  All rights reserved.
  ----------------------------------------------------------
}

interface

uses
    Classes,
    Command,
    DSSClass,
    Circuit,
    PDElement,
    UcMatrix,
    LineCode,
    ArrayDef,
    LineGeometry,
    LineSpacing,
    ConductorData,
    PDClass,
    UComplex, DSSUcomplex,
    DSSObject;

type
{$SCOPEDENUMS ON}
    TLineProp = (
        INVALID = 0,
        bus1 = 1,
        bus2 = 2,
        linecode = 3,
        length = 4,
        phases = 5,
        r1 = 6,
        x1 = 7,
        r0 = 8,
        x0 = 9,
        C1 = 10,
        C0 = 11,
        rmatrix = 12,
        xmatrix = 13,
        cmatrix = 14,
        Switch = 15,
        Rg = 16,
        Xg = 17,
        rho = 18,
        geometry = 19,
        units = 20,
        spacing = 21,
        wires = 22,
        EarthModel = 23,
        cncables = 24,
        tscables = 25,
        B1 = 26,
        B0 = 27,
        Seasons = 28,
        Ratings = 29,
        LineType = 30
    );
{$SCOPEDENUMS OFF}

    TLine = class(TPDClass)
    PROTECTED
        procedure DefineProperties; override;
    PUBLIC
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;

        function EndEdit(ptr: Pointer; const NumChanges: integer): Boolean; override;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLineObj = class(TPDElement)
    PUBLIC
        FZFrequency: Double; // keep track of last frequency computed for geometry
        FLineCodeUnits: Integer;
        FUnitsConvert: Double; // conversion factor
        FWireDataSize: Integer;
        FPhaseChoice: ConductorChoice;
        FEarthModel: Integer;

        FrhoSpecified: Boolean;
        FCapSpecified: Boolean; // To make sure user specifies C in some form
        SymComponentsChanged: Boolean;
        SymComponentsModel: LongBool;
        IsSwitch: LongBool;

        FLineType: Integer; // Pointer to code for type of line
        UserLengthUnits: Integer; // keep track of the user's input length units
        Zinv: TCMatrix;
        Z: TCMatrix;   // Base Frequency Series Z matrix  per unit length
        Yc: TCMatrix;

        R1: Double;
        X1: Double;
        R0: Double;
        X0: Double;
        C1: Double;
        C0: Double;
        Len: Double;
        LengthUnits: Integer;

        Rg, Xg, KXg, rho: Double;
        GeneralPlotQuantity: Double;  // For general circuit plotting
        
        LineCodeObj: TLineCodeObj;
        LineGeometryObj: TLineGeometryObj;
        LineSpacingObj: TLineSpacingObj;

        FLineWireData: pConductorDataArray;

        procedure FMakeZFromGeometry(f: Double); // make new Z, Zinv, Yc, etc
        procedure KillGeometrySpecified;

        procedure FMakeZFromSpacing(f: Double); // make new Z, Zinv, Yc, etc
        procedure KillSpacingSpecified;

        procedure ClearYPrim;
        procedure ResetLengthUnits;

        function NumConductorData: Integer;
        function FetchConductorData(i: Integer): TConductorDataObj;

        procedure ReallocZandYcMatrices;

        procedure DoLongLine(Frequency: Double; var R1_h: Double; var X1_h: Double; var C1_h: Double; var G1_h: Double);  // Long Line Correction for 1=phase
        procedure ConvertZinvToPosSeqR;  // for GIC analysis, primarily

        // procedure GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex); OVERRIDE;
        procedure GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroSeqLosses: complex); OVERRIDE;

        constructor Create(ParClass: TDSSClass; const LineName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer = 0); override;
        procedure MakeLike(OtherPtr: Pointer); override;

        procedure RecalcElementData; OVERRIDE;
        procedure CalcYPrim; OVERRIDE;

        procedure MakePosSequence(); OVERRIDE;  // Make a positive Sequence Model
        function MergeWith(var Other: TLineObj; Series: Boolean): Boolean;
        procedure UpdateControlElements(NewLine, OldLine: TLineObj);

        procedure DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;

        // Public for the COM Interface
        procedure FetchLineCode();
        procedure FetchGeometryCode();
        procedure FetchLineSpacing();

        // Reliability calcs
        procedure CalcFltRate; OVERRIDE;  // Calc failure rates for section and buses

        // CIM XML access
        function LineCodeSpecified: Boolean;
        function GeometrySpecified: Boolean;
        function SpacingSpecified: Boolean;

        property PhaseChoice: ConductorChoice READ FPhaseChoice;

        property UnitsConvert: Double READ FUnitsConvert;  // conversion to present Line units

        property NumConductorsAvailable: Integer READ NumConductorData;
        property ConductorData[i: Integer]: TConductorDataObj READ FetchConductorData;
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    Sysutils,
    Utilities,
    Mathutil,
    ControlElem,
    LineUnits,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TLineObj;
    TProp = TLineProp;
const
    NumPropsThisClass = Ord(High(TProp));
    CAP_EPSILON: Complex = (re: 0.0; im: 4.2e-8); // 5 kvar of capacitive reactance at 345 kV to avoid open line problem
    ONE_THIRD = (1.0 / 3.0);
var
    PropInfo: Pointer = NIL;    

constructor TLine.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
        PropInfo := TypeInfo(TProp);

    inherited Create(dssContext, LINE_ELEMENT, 'Line');
end;

destructor TLine.Destroy;
begin
    inherited Destroy;
end;

function GetZSeqScale(obj: TLineObj; getter: Boolean): Double;
begin
    if getter then
        Result := obj.FUnitsConvert
    else
        Result := 1;
end;

function GetCSeqScale(obj: TLineObj; getter: Boolean): Double;
begin
    if getter then
        Result := obj.FUnitsConvert * 1.0e-9
    else
        Result := 1.0e-9;
end;

function GetZmatScale(obj: TLineObj; getter: Boolean): Double;
begin
    Result := 1;
    if getter then
    begin
        if obj.GeometrySpecified or obj.SpacingSpecified then
            Result := Result * obj.Len
        else
            Result := Result * obj.FUnitsConvert
    end;
end;

function GetYCScale(obj: TLineObj; getter: Boolean): Double;
begin
    Result := TwoPi * obj.BaseFrequency * 1.0e-9;
    if getter then
    begin
        if obj.GeometrySpecified or obj.SpacingSpecified then
            Result := Result * obj.Len
        else
            Result := Result * obj.FUnitsConvert
    end;
end;

function GetB1B0Scale(obj: TLineObj): Double; //TODO: why no FUnitsConvert here?
begin
    Result := 1 / (TwoPi * obj.BaseFrequency) * 1.0e-6;
end;

procedure SetWires(Obj: TObj; Value: TDSSObjectPtr; ValueCount: Integer); forward;

procedure TLine.DefineProperties;
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo);

    // list of objects
    PropertyStructArrayCountOffset := ptruint(@obj.FWireDataSize);
    //PropertyStructArrayIndexOffset := ptruint(@obj.FActiveCond);

    PropertyType[ord(TProp.tscables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.tscables)] := ptruint(@obj.FLineWireData);
    PropertyOffset2[ord(TProp.tscables)] := ptruint(DSS.TSDataClass);
    PropertyFlags[ord(TProp.tscables)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.tscables)] := ord(TProp.wires);

    PropertyType[ord(TProp.cncables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.cncables)] := ptruint(@obj.FLineWireData);
    PropertyOffset2[ord(TProp.cncables)] := ptruint(DSS.CNDataClass);
    PropertyFlags[ord(TProp.cncables)] := [TPropertyFlag.Redundant];
    PropertyRedundantWith[ord(TProp.cncables)] := ord(TProp.wires);

    PropertyType[ord(TProp.wires)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.wires)] := ptruint(@obj.FLineWireData);
    PropertyOffset2[ord(TProp.wires)] := ptruint(DSS.WireDataClass);
    PropertyWriteFunction[ord(TProp.wires)] := @SetWires;
    PropertyFlags[ord(TProp.wires)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.FullNameAsArray];

    // matrices
    PropertyType[ord(TProp.rmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.rmatrix)] := ptruint(@obj.Z);
    PropertyOffset2[ord(TProp.rmatrix)] := ptruint(@GetZmatScale);
    PropertyFlags[ord(TProp.rmatrix)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.RealPart];
    
    PropertyType[ord(TProp.xmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.xmatrix)] := ptruint(@obj.Z);
    PropertyOffset2[ord(TProp.xmatrix)] := ptruint(@GetZmatScale);
    PropertyFlags[ord(TProp.xmatrix)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.ImagPart];

    PropertyType[ord(TProp.cmatrix)] := TPropertyType.ComplexPartSymMatrixProperty;
    PropertyOffset[ord(TProp.cmatrix)] := ptruint(@obj.YC);
    PropertyOffset2[ord(TProp.cmatrix)] := ptruint(@GetYCScale);
    PropertyFlags[ord(TProp.cmatrix)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.ImagPart];

    // integer properties
    PropertyType[ord(TProp.phases)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.phases)] := ptruint(@obj.FNPhases);
    PropertyFlags[ord(TProp.phases)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];

    // enums
    PropertyType[ord(TProp.units)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.units)] := ptruint(@obj.LengthUnits);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);

    PropertyType[ord(TProp.linetype)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.linetype)] := ptruint(@obj.FLineType);
    PropertyOffset2[ord(TProp.linetype)] := PtrInt(DSS.LineTypeEnum);

    PropertyType[ord(TProp.EarthModel)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.EarthModel)] := ptruint(@obj.FEarthModel);
    PropertyOffset2[ord(TProp.EarthModel)] := PtrInt(DSS.EarthModelEnum);

    // object properties
    PropertyType[ord(TProp.linecode)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.geometry)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.spacing)] := TPropertyType.DSSObjectReferenceProperty;
    
    PropertyOffset[ord(TProp.linecode)] := ptruint(@obj.LineCodeObj);
    PropertyOffset[ord(TProp.geometry)] := ptruint(@obj.LineGeometryObj);
    PropertyOffset[ord(TProp.spacing)] := ptruint(@obj.LineSpacingObj);

    PropertyOffset2[ord(TProp.linecode)] := ptruint(DSS.LineCodeClass);
    PropertyOffset2[ord(TProp.geometry)] := ptruint(DSS.LineGeometryClass);
    PropertyOffset2[ord(TProp.spacing)] := ptruint(DSS.LineSpacingClass);

    // double arrays
    PropertyType[ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Ratings)] := ptruint(@obj.AmpRatings);
    PropertyOffset2[ord(TProp.Ratings)] := ptruint(@obj.NumAmpRatings);

    // bus properties
    PropertyType[ord(TProp.bus1)] := TPropertyType.BusProperty;
    PropertyType[ord(TProp.bus2)] := TPropertyType.BusProperty;
    PropertyOffset[ord(TProp.bus1)] := 1;
    PropertyOffset[ord(TProp.bus2)] := 2;

    // boolean properties
    PropertyType[ord(TProp.Switch)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.Switch)] := ptruint(@obj.IsSwitch);

    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);

    // double properties (default type)
    PropertyOffset[ord(TProp.length)] := ptruint(@obj.Len);
    PropertyOffset[ord(TProp.r1)] := ptruint(@obj.r1);
    PropertyOffset[ord(TProp.x1)] := ptruint(@obj.x1);
    PropertyOffset[ord(TProp.r0)] := ptruint(@obj.r0);
    PropertyOffset[ord(TProp.x0)] := ptruint(@obj.x0);
    PropertyOffset[ord(TProp.Rg)] := ptruint(@obj.Rg);
    PropertyOffset[ord(TProp.Xg)] := ptruint(@obj.Xg);
    PropertyOffset[ord(TProp.rho)] := ptruint(@obj.Rho);
    PropertyOffset[ord(TProp.C1)] := ptruint(@obj.C1);
    PropertyOffset[ord(TProp.C0)] := ptruint(@obj.C0);

    PropertyOffset[ord(TProp.B1)] := ptruint(@obj.C1);
    PropertyOffset2[ord(TProp.B1)] := ptruint(@GetB1B0Scale);
    PropertyFlags[ord(TProp.B1)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.Redundant, TPropertyFlag.ConditionalValue];
    PropertyRedundantWith[ord(TProp.B1)] := ord(TProp.C1);

    PropertyOffset[ord(TProp.B0)] := ptruint(@obj.C0);
    PropertyOffset2[ord(TProp.B0)] := ptruint(@GetB1B0Scale);
    PropertyFlags[ord(TProp.B0)] := [TPropertyFlag.ScaledByFunction, TPropertyFlag.Redundant, TPropertyFlag.ConditionalValue];
    PropertyRedundantWith[ord(TProp.B0)] := ord(TProp.C0);

    PropertyFlags[ord(TProp.r1)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.ScaledByFunction];
    PropertyFlags[ord(TProp.x1)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.ScaledByFunction];
    PropertyFlags[ord(TProp.C1)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.ScaledByFunction];
    PropertyFlags[ord(TProp.r0)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.ScaledByFunction];
    PropertyFlags[ord(TProp.x0)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.ScaledByFunction];
    PropertyFlags[ord(TProp.C0)] := [TPropertyFlag.ConditionalValue, TPropertyFlag.ScaledByFunction];

    // We could also just remove ConditionalValue here and use NaN as 
    // the scale to achieve the same effect

    PropertyOffset3[ord(TProp.r1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.x1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.C1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.B1)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.r0)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.x0)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.C0)] := ptruint(@obj.SymComponentsModel);
    PropertyOffset3[ord(TProp.B0)] := ptruint(@obj.SymComponentsModel);

    PropertyOffset2[ord(TProp.r1)] := ptruint(@GetZSeqScale);
    PropertyOffset2[ord(TProp.x1)] := ptruint(@GetZSeqScale);
    PropertyOffset2[ord(TProp.r0)] := ptruint(@GetZSeqScale);
    PropertyOffset2[ord(TProp.x0)] := ptruint(@GetZSeqScale);
    PropertyOffset2[ord(TProp.c1)] := ptruint(@GetCSeqScale);
    PropertyOffset2[ord(TProp.c0)] := ptruint(@GetCSeqScale);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties;
end;

function TLine.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    Obj: TObj;
begin
    Obj := TObj.Create(Self, ObjName);
    if Activate then 
        ActiveCircuit.ActiveCktElement := Obj;
    Obj.ClassIndex := AddObjectToList(Obj, Activate);
    Result := Obj;
end;

procedure TLineObj.FetchLineCode();
var
    i: Integer;
begin
    if LineCodeObj = NIL then
        Exit;

    // Frequency compensation takes place in calcYPrim.
    BaseFrequency := LineCodeObj.BaseFrequency;
    // Copy impedances from line code, but do not recalc because symmetrical
    // component z's may not match what's in matrix
    if LineCodeObj.SymComponentsModel then
    begin
        R1 := LineCodeObj.R1;
        X1 := LineCodeObj.X1;
        R0 := LineCodeObj.R0;
        X0 := LineCodeObj.X0;
        C1 := LineCodeObj.C1;
        C0 := LineCodeObj.C0;
        SymComponentsModel := TRUE;
    end
    else
        SymComponentsModel := FALSE;

    // Earth return impedances used to compensate for frequency
    Rg := LineCodeObj.Rg;
    Xg := LineCodeObj.Xg;
    rho := LineCodeObj.rho;
    Kxg := Xg / ln(658.5 * sqrt(rho / BaseFrequency));

    FLineCodeUnits := LineCodeObj.Units;

    FUnitsConvert := ConvertLineUnits(FLineCodeUnits, LengthUnits);

    NormAmps := LineCodeObj.NormAmps;
    EmergAmps := LineCodeObj.EmergAmps;

    NumAmpRatings := LineCodeObj.NumAmpRatings;
    setlength(AmpRatings, NumAmpRatings);
    for i := 0 to High(AmpRatings) do
        AmpRatings[i] := LineCodeObj.AmpRatings[i];

    // These three properties should not come from the Linecode
    //   But can vary from line section to line section
    // commented out 8/26/2014
    // FaultRate := LineCodeObj.FaultRate;
    // PctPerm   := LineCodeObj.PctPerm;
    // HrsToRepair := LineCodeObj.HrsToRepair;

    SetAsNextSeq(ord(TProp.Ratings));

    if Fnphases <> LineCodeObj.FNphases then
    begin
        FNPhases := LineCodeObj.FNPhases;

        ReallocZandYcMatrices;
    end;

    if not SymComponentsModel then
    begin        // Copy matrices
        Z.CopyFrom(LineCodeObj.Z);
        // Zinv.CopyFrom(LineCodeObj.Zinv);  // no need to copy Zinv
        Yc.CopyFrom(LineCodeObj.Yc);
    end
    else
        RecalcElementData;    // Compute matrices

    NConds := Fnphases;  // Force Reallocation of terminal info
    //Fnconds := Fnphases;
    Yorder := Fnconds * Fnterms;
    // YPrimInvalid := True;  (set in Edit; this is redundant)

    FLineType := LineCodeObj.FLineType;

    KillSpacingSpecified;
    KillGeometrySpecified;
end;

procedure TLineObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer);
begin
    case Idx of
        ord(TProp.C1), ord(TProp.C0), ord(TProp.cmatrix), ord(TProp.B1), ord(TProp.B0):
            FCapSpecified := TRUE;
        ord(TProp.cncables):
        begin
            LineCodeObj := NIL;
            KillGeometrySpecified;
            FPhaseChoice := ConcentricNeutral;
        end;
        ord(TProp.tscables):
        begin
            LineCodeObj := NIL;
            KillGeometrySpecified;
            FPhaseChoice := TapeShield;
        end;
        ord(TProp.units):
        begin // Update units conversion factor that might have been changed previously
            if LineCodeSpecified then
                FUnitsConvert := ConvertLineUnits(FLineCodeUnits, LengthUnits)
            else
                FUnitsConvert := FUnitsConvert * ConvertLineUnits(previousIntVal, LengthUnits);
            
            UserLengthUnits := LengthUnits;
        end;
    end;

    case Idx of
        ord(TProp.linecode):
            FetchLineCode();  // Define line by conductor code
        ord(TProp.length), ord(TProp.units):     // for Reliability calcs -- see PDElement.Pas
            MilesThisLine := len * ConvertLineUnits(LengthUnits, UNITS_MILES);
        ord(TProp.phases): // Change the number of phases ... only valid if SymComponentsModel=TRUE
            if Fnphases <> previousIntVal then
            begin
                if (not GeometrySpecified) and SymComponentsModel then
                begin  
                    NConds := Fnphases;  // Force Reallocation of terminal info
                    Yorder := Fnterms * Fnconds;
                    // YPrimInvalid := True;  // now set below
                    RecalcElementData;  // Reallocate Z, etc.
                end
                else
                begin
                    // ignore change of nphases if geometry used                
                    FNphases := previousIntVal;
                    DoSimpleMsg('Illegal change of number of phases for "%s"', [FullName], 18101);
                end;
            end;
        ord(TProp.r1),
        ord(TProp.x1),
        ord(TProp.r0),
        ord(TProp.x0),
        ord(TProp.C1),
        ord(TProp.C0),
        ord(TProp.B1),
        ord(TProp.B0):
        begin
            LineCodeObj := NIL;
            KillGeometrySpecified;
            KillSpacingSpecified;
            ResetLengthUnits;
            SymComponentsChanged := TRUE;
            SymComponentsModel := TRUE;
        end;
        ord(TProp.rmatrix), ord(TProp.xmatrix), ord(TProp.cmatrix):
        begin
            LineCodeObj := NIL;
            SymComponentsModel := FALSE;
            ResetLengthUnits;
            KillGeometrySpecified;
            KillSpacingSpecified;
        end;
        ord(TProp.Switch):
            if IsSwitch then
            begin
                SymComponentsChanged := TRUE;
                YprimInvalid := TRUE;
                KillGeometrySpecified();
                KillSpacingSpecified();
                r1 := 1.0;
                x1 := 1.0;
                r0 := 1.0;
                x0 := 1.0;
                c1 := 1.1 * 1.0e-9;
                c0 := 1.0 * 1.0e-9;
                len := 0.001;
                ResetLengthUnits;
            end;
        ord(TProp.Xg), ord(TProp.rho):
            Kxg := Xg / ln(658.5 * sqrt(rho / BaseFrequency));
        ord(TProp.geometry):
            FetchGeometryCode();
        ord(TProp.spacing), ord(TProp.wires), ord(TProp.cncables), ord(TProp.tscables):
        begin
            if Idx = ord(TProp.spacing) then
                FetchLineSpacing();
            if Assigned(LineSpacingObj) and Assigned(FLineWireData) then
            begin
                SymComponentsModel := FALSE;
                SymComponentsChanged := FALSE;
                KillGeometrySpecified;
            end;
            YprimInvalid := TRUE;
        end;
        ord(TProp.Seasons):
            setlength(AmpRatings, NumAmpRatings);
    end;

    case Idx of
        3..14: // TODO: check -- looks incomplete
            //YPrim invalidation on anything that changes impedance values
            YprimInvalid := TRUE;
        ord(TProp.rho):
        begin
            FrhoSpecified := TRUE;
            if GeometrySpecified then
                LineGeometryObj.rhoearth := rho; // TODO: This is weird
        end;
    end;
    inherited PropertySideEffects(Idx, previousIntVal);
end;

function TLine.EndEdit(ptr: Pointer; const NumChanges: integer): Boolean;
begin
    // We need to override this one since Line doesn't call ReCalcData in 
    // the original codebase
    Exclude(TDSSObject(ptr).Flags, Flg.EditionActive);
    Result := True;
end;

procedure SetWires(Obj: TObj; Value: TDSSObjectPtr; ValueCount: Integer);
var
    RatingsInc: Boolean;
    NewNumRat, i, istart: Integer;
    NewRatings: array of Double;
begin
    with Obj do
    begin
        // Previously in "FetchWireList"
        if not assigned(LineSpacingObj) then
        begin
            DoSimpleMsg('You must assign the LineSpacing before the Wires Property ("%s").', [FullName], 18102);
            Exit;
        end;

        if FPhaseChoice = Unknown then
        begin // it's an overhead line
            LineCodeObj := NIL;
            KillGeometrySpecified;
            istart := 1;
            FPhaseChoice := Overhead;
        end
        else
        begin // adding bare neutrals to an underground line - TODO what about repeat invocation?
            istart := LineSpacingObj.NPhases + 1;
        end;

        NewNumRat := 1;
        RatingsInc := FALSE; // So far we don't know if there are seasonal ratings

        // Validate number of elements
        if (LineSpacingObj.NWires - istart + 1) <> ValueCount then
        begin
            DoSimpleMsg('%s: Unexpected number (%d) of wires; expected %d objects.', 
                [FullName, ValueCount, (LineSpacingObj.NWires - istart + 1)], 18102);
            Exit;
        end;

        for i := istart to LineSpacingObj.NWires do
        begin
            FLineWireData[i] := TConductorDataObj(Value^);
            if FLineWireData[i].NumAmpRatings > NewNumRat then
            begin
                NewNumRat := FLineWireData[i].NumAmpRatings;
                NewRatings := Copy(FLineWireData[i].AmpRatings, 0, NewNumRat);
                RatingsInc := TRUE; // Yes, there are seasonal ratings
            end;
            NormAmps := FLineWireData[i].NormAmps;
            EmergAmps := FLineWireData[i].EmergAmps;
            Inc(Value);
        end;

        if RatingsInc then
        begin
            NumAmpRatings := NewNumRat;
            AmpRatings := NewRatings;
        end;

        SetAsNextSeq(ord(TProp.Ratings));
    end;
end;

// A Line Defaults to 3-phases and some typical symmetrical component data
//
// Line impedances are specified in per unit length and are multiplied by the length
// when the primitive Y matrix is computed.
//
// You may specify the impedances of the line either by symmetrical components or
// by R, X, and nodal C matrices  (also per unit length).
//
// All C's is entered in nano farads.
//
// The ultimate values are in the matrices.  If you specify matrices, then the symmetrical
// component values are ignored.  However, if you change any of the symmetrical component values
// the matrices will be recomputed.  It is assumed you want to use symmetrical component values.
// Don't mix data entry by matrix and by symmetrical components.
//
// Note that if you change the number of phases, the matrices are reallocated and reinitialized
// with whatever is currently in the symmetrical component data.

procedure TLineObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
begin
    inherited MakeLike(OtherPtr);  // Take care of inherited class properties

    Other := TObj(OtherPtr);
    if Fnphases <> Other.Fnphases then
    begin
        FNphases := Other.Fnphases;
        NConds := Fnphases; // force reallocation of terminals and conductors

        Yorder := Fnconds * Fnterms;
        YPrimInvalid := TRUE;

        if Z <> NIL then
            Z.Free;
        if Zinv <> NIL then
            Zinv.Free;
        if Yc <> NIL then
            Yc.Free;

        // For a line, nphases = ncond, for now
        Z := TCmatrix.CreateMatrix(Fnphases);
        Zinv := TCMatrix.CreateMatrix(Fnphases);
        Yc := TCMatrix.CreateMatrix(Fnphases);
    end;

    Z.CopyFrom(Other.Z);
    // Zinv.CopyFrom(Other.Zinv);
    Yc.CopyFrom(Other.Yc);

    R1 := Other.R1;
    X1 := Other.X1;
    R0 := Other.R0;
    X0 := Other.X0;
    C1 := Other.C1;
    C0 := Other.C0;
    Len := Other.Len;

    SymComponentsModel := Other.SymComponentsModel;
    FCapSpecified := Other.FCapSpecified;
end;

constructor TLineObj.Create(ParClass: TDSSClass; const LineName: String);
begin
    inherited Create(ParClass);

    Name := AnsiLowerCase(LineName);
    DSSObjType := ParClass.DSSClassType; // DSSObjType + LINESECTION; // in both PDElement list and Linesection lists

    FNphases := 3;  // Directly set conds and phases
    Fnconds := 3;
    Nterms := 2;  // Force allocation of terminals and conductors
    IsSwitch := FALSE;
    R1 := 0.0580;  //ohms per 1000 ft
    X1 := 0.1206;
    R0 := 0.1784;
    X0 := 0.4047;
    C1 := 3.4e-9;  // nf per 1000ft
    C0 := 1.6e-9;
    Len := 1.0;   // 1 kFt
    Z := NIL;
    Zinv := NIL;
    Yc := NIL;
    LineCodeObj := NIL;

    Rg := 0.01805;    //ohms per 1000 ft
    Xg := 0.155081;
    rho := 100.0;
    Kxg := Xg / ln(658.5 * sqrt(rho / BaseFrequency));
    FrhoSpecified := FALSE;
    FCapSpecified := FALSE;

     {Basefrequency := 60.0;}  // set in base class
    Normamps := 400.0;
    EmergAmps := 600.0;
    PctPerm := 20.0;
    FaultRate := 0.1; // per mile per year
    HrsToRepair := 3.0;

    SymComponentsChanged := FALSE;
    SymComponentsModel := TRUE;

    LineGeometryObj := NIL;
    LengthUnits := UNITS_NONE; // Assume everything matches
    UserLengthUnits := UNITS_NONE;
    FUnitsConvert := 1.0;
    FLineCodeUnits := UNITS_NONE;
    FEarthModel := DSS.DefaultEarthModel;
    FLineType := 1;  // Default to OH Line

    LineSpacingObj := NIL;
    FLineWireData := NIL;
    FWireDataSize := 0;
    FPhaseChoice := Unknown;

    FZFrequency := -1.0; // indicate Z not computed.

    Yorder := Fnterms * Fnconds;
    RecalcElementData;

    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;
end;

destructor TLineObj.Destroy;
begin
    if Assigned(Z) then
        Z.Free;
    if Assigned(Zinv) then
        Zinv.Free;
    if Assigned(Yc) then
        Yc.Free;
    Reallocmem(FLineWireData, 0);
    inherited destroy;
end;

procedure TLineObj.ReallocZandYcMatrices;
begin
    if (Z <> NIL) and (Z.Order = Fnphases) then 
        Exit;

    if Z <> NIL then
        Z.Free;
    if Zinv <> NIL then
        Zinv.Free;
    if Yc <> NIL then
        Yc.Free;

    // For a line, nphases = ncond, for now
    Z := TCmatrix.CreateMatrix(Fnphases);
    Zinv := TCMatrix.CreateMatrix(Fnphases);
    Yc := TCMatrix.CreateMatrix(Fnphases);
end;

procedure TLineObj.DoLongLine(Frequency: Double; var R1_h: Double; var X1_h: Double; var C1_h: Double; var G1_h: Double);
// Do long line correction for len and desired frequency
// Updated the procedure to correct for any frequency. Moved usage to CalcYPrim.
var
    Zs, Zm, Ys, Ym, Zc: Complex;
    GammaL, ExpP, ExpM, SinhGL, CoshGL: Complex;
begin
    G1_h := EPSILON; // Adding a tiny conductance to avoid skipping correction on lines with C1=0
    Zs := cmplx(R1 * Len, X1 * Len * Frequency / BaseFrequency);  // Use X1 for the desired frequency

    Ys := cmplx(G1_h, TwoPi * Frequency * C1 * Len);
        // Apply the long-line correction to obtain Zm and Ym
        // Rearrange things to express as in Arrillaga's book. no difference to original DoLongLine.
    GammaL := Csqrt(Zs * Ys);
    Zc := Csqrt(Zs / Ys);
    ExpP := cmplx(cos(GammaL.im), sin(GammaL.im)) * exp(GammaL.re);
    ExpM := Cinv(ExpP);

    SinhGL := (ExpP - ExpM) * 0.5;
    CoshGL := (ExpP + ExpM) * 0.5;
    Zm := Zc * SinhGL;
    Ym := (Cinv(Zc) * ((CoshGL - cmplx(1.0, 0.0)) / SinhGL)) * 2.0;

        // Update values for tested frequency and adjusted for long-line
        // do not replace original X1, R1, C1. We use these locally where the procedure is called.
    R1_h := Zm.re / Len;
    X1_h := Zm.im / Len;  // X1_h output is already accounting for new frequency
    C1_h := Ym.im / Len / TwoPi / Frequency;
    G1_h := Ym.re
end;

procedure TLineObj.RecalcElementData;
//  This routine is only called when the symmetrical component data have changed
//  It computes the values for Z and Yc in ohms per unit length
//
//  Can also compute long line correction for 1-phase pos sequence line models
var
    Zs, Zm, Ys, Ym, Ztemp: Complex;
    i, j: Integer;
    Yc1, Yc0: Double;

begin
    ReallocZandYcMatrices;

    // Only time this is called is if symmetrical components are specified

    Ztemp := cmplx(R1, X1) * 2.0;
    // Handle special case for 1-phase line and/or pos seq model
    if (FnPhases = 1) or ActiveCircuit.PositiveSequence then
    begin
        // Long line correction has been moved to CalcYPrim as it must be performed frequency-wise (not only for base freq).
        // Zero sequence the same as positive sequence
        R0 := R1;
        X0 := X1;
        C0 := C1;
    end;

    Zs := (Ztemp + Cmplx(R0, X0)) * ONE_THIRD;
    Zm := (cmplx(R0, X0) - Cmplx(R1, X1)) * ONE_THIRD;

    Yc1 := TwoPi * BaseFrequency * C1;
    Yc0 := TwoPi * BaseFrequency * C0;

    Ys := (Cmplx(0.0, Yc1) * 2 + Cmplx(0.0, Yc0)) * ONE_THIRD;
    Ym := (cmplx(0.0, Yc0) - Cmplx(0.0, Yc1)) * ONE_THIRD;

    for i := 1 to Fnphases do
    begin
        Z.SetElement(i, i, Zs);
        Yc.SetElement(i, i, Ys);
        for j := 1 to i - 1 do
        begin
            Z.SetElemsym(i, j, Zm);
            Yc.SetElemsym(i, j, Ym);
        end;
    end;

    SymComponentsChanged := FALSE;

    // values in ohms per unit length
end;

procedure TLineObj.CalcFltRate;
begin
    // inherited;
    // Assume Faultrate specified in same units as length
    BranchFltRate := Faultrate * pctperm * 0.01 * Len;
end;

procedure TLineObj.CalcYPrim;
var
    Value: Complex;
    ZinvValues: pComplexArray;
    ZValues: pComplexArray;
    YValues: pComplexArray;

    FreqMultiplier: Double;
    XgMod: Double;
    LengthMultiplier: Double;

    i, j, k, Norder: Integer;

    // Andres Ovalle: Added a few for long-line correction frequency-wise
    Zs, Zm, Ys, Ym, Ztemp: Complex;
    Yc1, Yc0, R1_h, X1_h, C1_h, G1_h, R0_h, X0_h, C0_h, G0_h: Double;
begin
    FreqMultiplier := 1.0;
    LengthMultiplier := 1.0;

    if SymComponentsChanged then
    begin
        //Try to catch inadvertent user error when they forget to specify C1 and C0
        //Check to see if user has spec'd C1 and C0. If not, adjust default values for new length units
        if not FCapSpecified then
        begin
            C1 := C1 / ConvertLineUnits(UNITS_KFT, LengthUnits); // were defined in kft
            C0 := C0 / ConvertLineUnits(UNITS_KFT, LengthUnits);
            FCapSpecified := TRUE;   // so we don't do it again
        end;

        RecalcElementData;
    end;

    ClearYPrim;

    // Build Series YPrim
    with YPrim_Series do
    begin
        // Build Zmatrix
        if GeometrySpecified then
        begin
            FMakeZFromGeometry(ActiveCircuit.Solution.Frequency); // Includes length in proper units
            if DSS.SolutionAbort then
                Exit;

        end
        else
        if SpacingSpecified then
        begin
            FMakeZFromSpacing(ActiveCircuit.Solution.Frequency); // Includes length in proper units
            if DSS.SolutionAbort then
                Exit;
        end
        else
        begin  // Z is from line code or specified in line data
               // In this section Z is assumed in ohms per unit length
            LengthMultiplier := Len / FUnitsConvert;   // convert to per unit length
            FYprimFreq := ActiveCircuit.Solution.Frequency;
            FreqMultiplier := FYprimFreq / BaseFrequency;

            // If positive sequence, long-line correction can be taken into account here
            // It needs to be recalculated for every frequency
            if ((FnPhases = 1) or ActiveCircuit.PositiveSequence) then
            begin
                // These values are specific for the harmonic being tested (adjust for frequency)
                R1_h := R1;
                X1_h := X1 * FYprimFreq / BaseFrequency; // Adjust for frequency here
                C1_h := C1;
                G1_h := 0.0;  // DoLongLine uses a tiny conductance to avoid skipping case where C1=0
                // long-line equivalent PI, but only for CktModel=Positive
                if ActiveCircuit.PositiveSequence then // A.Ovalle: To avoid errors in higher freqs, we shouldn't skip cases with C1=0. Critical to match IEEE 14bus harmonics benchmark.
                begin
                    // do long-line correction for tested frequency
                    // use R1_h, X1_h, C1_h, G1_h to correct Y prim
                    DoLongLine(FYprimFreq, R1_h, X1_h, C1_h, G1_h);
                end;
                  // zero sequence the same as positive sequence
                R0_h := R1_h;
                X0_h := X1_h;
                C0_h := C1_h;
                G0_h := G1_h;
                Ztemp := cmplx(R1_h, X1_h) * 2.0;
                Zs := (Ztemp + Cmplx(R0_h, X0_h)) * ONE_THIRD;
                Zm := (cmplx(R0_h, X0_h) - Cmplx(R1_h, X1_h)) * ONE_THIRD;

                Yc1 := TwoPi * FYprimFreq * C1_h;
                Yc0 := TwoPi * FYprimFreq * C0_h;

                Ys := (Cmplx(G1_h, Yc1) * 2.0 + Cmplx(G0_h, Yc0)) * ONE_THIRD;
                Ym := (cmplx(G0_h, Yc0) - Cmplx(G1_h, Yc1)) * ONE_THIRD;

                for i := 1 to Fnphases do
                begin
                    Z.SetElement(i, i, Zs);
                    Yc.SetElement(i, i, Ys);
                    for j := 1 to i - 1 do
                    begin
                        Z.SetElemsym(i, j, Zm);
                        Yc.SetElemsym(i, j, Ym);
                    end;
                end;

                // Put in Series RL
                ZValues := Z.GetValuesArrayPtr(Norder);
                ZinvValues := Zinv.GetValuesArrayPtr(Norder);
                // Correct the impedances for length and frequency
                // Rg increases with frequency
                // Xg modified by ln of sqrt(1/f)
                if Xg <> 0.0 then
                    Xgmod := 0.5 * KXg * ln(FreqMultiplier)
                else
                    Xgmod := 0.0;

                for i := 1 to Norder * Norder do
                     // Apply freq multiplier only to Xgmod as we have already accounted for freq adjustment above
                    ZinvValues^[i] := Cmplx((ZValues^[i].re + Rg * (FreqMultiplier - 1.0)) * LengthMultiplier, (ZValues^[i].im - Xgmod * FreqMultiplier) * LengthMultiplier);

            end
            else
            begin
                // Original piece of code is kept here
                
                // Put in Series RL 
                ZValues := Z.GetValuesArrayPtr(Norder);
                ZinvValues := Zinv.GetValuesArrayPtr(Norder);
                 // Correct the impedances for length and frequency
                 // Rg increases with frequency
                 // Xg modified by ln of sqrt(1/f)
                if Xg <> 0.0 then
                    Xgmod := 0.5 * KXg * ln(FreqMultiplier)
                else
                    Xgmod := 0.0;

                for i := 1 to Norder * Norder do
                    ZinvValues^[i] := Cmplx((ZValues^[i].re + Rg * (FreqMultiplier - 1.0)) * LengthMultiplier, (ZValues^[i].im - Xgmod) * LengthMultiplier * FreqMultiplier);

            end;
            
            Zinv.Invert;  // Invert Z in-place to get values to put in Yprim
        end;

        // At this point have Z and Zinv in proper values including length
        // If GIC simulation, convert Zinv back to sym components, R Only

        if ActiveCircuit.Solution.Frequency < 0.51 then     // 0.5 Hz is cutoff
            ConvertZinvToPosSeqR;

        if Zinv.Inverterror > 0 then
        begin
            // If error, put in tiny series conductance
// TEMc - shut this up for the CDPSM connectivity profile test, or whenever else it gets annoying
            DoErrorMsg('TLineObj.CalcYPrim', 
                Format(_('Matrix Inversion Error for Line "%s"'), [Name]),
                _('Invalid impedance specified. Replaced with tiny conductance.'), 183);
            Zinv.Clear;
            for i := 1 to Fnphases do
                Zinv.SetElement(i, i, Cmplx(epsilon, 0.0));
        end
        else
            // Now, Put in Yprim_Series matrix 
            for i := 1 to Fnphases do
            begin
                for j := 1 to Fnphases do
                begin
                    Value := Zinv.GetElement(i, j);
                    SetElement(i, j, Value);
                    SetElement(i + Fnphases, j + Fnphases, Value);
                    Value := -Value;
                    SetElemSym(i, j + Fnphases, Value);
                end;
            end;


    end;

    YPrim.Copyfrom(Yprim_Series);      // Initialize YPrim for series impedances

    // Increase diagonal elements of both sides of line so that we will avoid isolated bus problem
    // add equivalent of 10 kvar capacitive at 345 kV
    with Yprim_Series do
        for i := 1 to Yorder do
            AddElement(i, i, CAP_EPSILON);

     // Now Build the Shunt admittances and add into YPrim
    if ActiveCircuit.Solution.Frequency > 0.51 then   // Skip Capacitance for GIC
        with YPrim_Shunt do
        begin
            // Put half the Shunt Capacitive Admittance at each end
            YValues := Yc.GetValuesArrayPtr(Norder);

            if GeometrySpecified or SpacingSpecified then
            begin
                // Values are already compensated for length and frequency
                k := 0;
                for j := 1 to Fnphases do
                    for i := 1 to Fnphases do
                    begin
                        Inc(k);    // Assume matrix in col order (1,1  2,1  3,1 ...)
                        Value := YValues^[k] / 2.0;  // half at each end ...
                        AddElement(i, j, Value);
                        AddElement(i + Fnphases, j + Fnphases, Value);
                    end;

            end
            else
            begin
                // Regular line model - values computed per unit length at base frequency
                k := 0;
                for j := 1 to Fnphases do
                    for i := 1 to Fnphases do
                    begin
                        Inc(k);    // Assume matrix in col order (1,1  2,1  3,1 ...)
                        if ((FnPhases = 1) or ActiveCircuit.PositiveSequence) then
                        begin
                            // If we enter here, frequency adjustment has already been applied above during Z and Yc recalculation (and also affected by long-line correction)
                            Value := Cmplx(YValues^[k].re / 2.0, YValues^[k].im * LengthMultiplier / 2.0);
                        end
                        else
                        begin
                            Value := Cmplx(0.0, YValues^[k].im * LengthMultiplier * FreqMultiplier / 2.0);
                        end;
                        AddElement(i, j, Value);
                        AddElement(i + Fnphases, j + Fnphases, Value);
                    end;
            end;

        // Now Account for Open Conductors
        // For any conductor that is open, zero out row and column

        end;

    YPrim.AddFrom(Yprim_Shunt);
    inherited CalcYPrim;
    YprimInvalid := FALSE;
end;

procedure TLineObj.DumpProperties(F: TFileStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
    rslt: String;
    LengthMult: Double;
begin
    inherited DumpProperties(F, Complete);

    with ParentClass do
    begin
        FSWriteln(F, '~ ' + PropertyName^[1] + '=' + firstbus);
        FSWriteln(F, '~ ' + PropertyName^[2] + '=' + nextbus);

        if LineCodeObj <> NIL then
            FSWriteln(F, '~ ' + PropertyName^[3] + '=' + LineCodeObj.Name)
        else
            FSWriteln(F, '~ ' + PropertyName^[3] + '=' + '');

        FSWriteln(F, Format('~ %s=%d', [PropertyName^[4], len]));
        FSWriteln(F, Format('~ %s=%d', [PropertyName^[5], Fnphases]));
        if SymComponentsModel then
            rslt := Format('%-.7g', [R1 / FUnitsConvert])
        else
            rslt := '----';
        FSWriteln(F, '~ ' + PropertyName^[6] + '=' + Rslt);
        if SymComponentsModel then
            rslt := Format('%-.7g', [X1 / FUnitsConvert])
        else
            rslt := '----';
        FSWriteln(F, '~ ' + PropertyName^[7] + '=' + Rslt);
        if SymComponentsModel then
            rslt := Format('%-.7g', [R0 / FUnitsConvert])
        else
            rslt := '----';
        FSWriteln(F, '~ ' + PropertyName^[8] + '=' + Rslt);
        if SymComponentsModel then
            rslt := Format('%-.7g', [X0 / FUnitsConvert])
        else
            rslt := '----';
        FSWriteln(F, '~ ' + PropertyName^[9] + '=' + Rslt);
        if SymComponentsModel then
            rslt := Format('%-.7g', [C1 * 1.0e9 / FUnitsConvert])
        else
            rslt := '----';
        FSWriteln(F, '~ ' + PropertyName^[10] + '=' + Rslt);
        if SymComponentsModel then
            rslt := Format('%-.7g', [C0 * 1.0e9 / FUnitsConvert])
        else
            rslt := '----';
        FSWriteln(F, '~ ' + PropertyName^[11] + '=' + Rslt);

        // If GeometrySpecified Or SpacingSpecified then length is embedded in Z and Yc    4-9-2020
        if GeometrySpecified or SpacingSpecified then
            LengthMult := Len
        else
            LengthMult := 1.0;

        FSWrite(F, '~ ' + PropertyName^[12] + '=' + '"');
        for i := 1 to Fnphases do
        begin
            for j := 1 to Fnphases do
            begin
                FSWrite(F, Format('%.9f ', [(Z.GetElement(i, j).re / LengthMult / FunitsConvert)]));
            end;
            FSWrite(F, '|');
        end;
        FSWriteln(F, '"');
        FSWrite(F, '~ ' + PropertyName^[13] + '=' + '"');
        for i := 1 to Fnphases do
        begin
            for j := 1 to Fnphases do
            begin
                FSWrite(F, Format('%.9f ', [(Z.GetElement(i, j).im / LengthMult / FunitsConvert)]));
            end;
            FSWrite(F, '|');
        end;
        FSWriteln(F, '"');
        FSWrite(F, '~ ' + PropertyName^[14] + '=' + '"');
        for i := 1 to Fnphases do
        begin
            for j := 1 to Fnphases do
            begin
                FSWrite(F, Format('%.3f ', [(Yc.GetElement(i, j).im / TwoPi / BaseFrequency / LengthMult / FunitsConvert * 1.0E9)]));
            end;
            FSWrite(F, '|');
        end;
        FSWriteln(F, '"');

        FSWrite(F, '~ ' + PropertyName^[15] + '=');
        FSWriteln(F, StrTOrF(IsSwitch));

        // Dump the rest by default
        for i := 16 to NumProperties do
        begin
            FSWriteln(F, '~ ' + PropertyName^[i] + '=' + PropertyValue[i]);
        end;
    end;
end;

// *********** Placeholder for Line module No Load Loss procedure *********
//procedure TLineObj.GetLosses(var TotalLosses, LoadLosses, NoLoadLosses: Complex);
//begin
//    // For Now, we'll just do the default behavior until we implement shunt losses
//    inherited;
//end;

procedure TLineObj.GetSeqLosses(var PosSeqLosses, NegSeqLosses, ZeroSeqLosses: complex);
// Only consider 3-phase branches with Pos seq >> Neg seq
// Otherwise, we don't know whether it is a 3-phase line or just a line with 3 phases
var
    i, j, k: Integer;
    Vph,
    V012,
    I012: array[0..2] of Complex;

begin
    PosSeqLosses := CZERO;
    NegSeqLosses := CZERO;
    ZeroSeqLosses := CZERO;

    // Method: sum seq powers going into each terminal
    
    if Fnphases = 3 then
    begin // 3-phase lines only
        ComputeIterminal;
        for i := 1 to 2 do
        begin
            k := (i - 1) * Fnphases + 1;
            for j := 0 to 2 do
                Vph[j] := ActiveCircuit.Solution.NodeV^[NodeRef^[k + j]];
            Phase2SymComp(pComplexArray(@Vph), pComplexArray(@V012));
            Phase2SymComp(pComplexArray(@Iterminal^[k]), pComplexArray(@I012));
            PosSeqLosses += V012[1] * cong(I012[1]);
            NegSeqLosses += V012[2] * cong(I012[2]); // accumulate both line modes
            ZeroSeqLosses += V012[0] * cong(I012[0]);
        end;
        PosSeqLosses *= 3.0;
        NegSeqLosses *= 3.0;
        ZeroSeqLosses *= 3.0;
    end;
end;

procedure TLineObj.MakePosSequence();
var
    C1_new, Cs, Cm: Double;
    LengthMult: Double;
    Z1, ZS, Zm: Complex;
    changes, i, j: Integer;
    NormAmps0: Double;
    EmergAmps0: Double;
    LengthUnits0: Integer;
begin
    NormAmps0 := NormAmps;
    EmergAmps0 := EmergAmps;
    LengthUnits0 := LengthUnits;

    // set to single phase and make sure R1, X1, C1 set.
    // If already single phase, let alone
    if FnPhases > 1 then
    begin
        BeginEdit(True);
        // Kill certain propertyvalue elements to get a cleaner looking save
        PrpSequence[3] := 0;
        for i := 6 to 14 do
            PrpSequence[i] := 0;

        // If GeometrySpecified Or SpacingSpecified then length is embedded in Z and Yc    4-9-2020
        if GeometrySpecified or SpacingSpecified then
            LengthMult := Len
        else
            LengthMult := 1.0;

        if IsSwitch then
        begin
            SetDouble(ord(TProp.R1), 1);
            SetDouble(ord(TProp.X1), 1);
            SetDouble(ord(TProp.C1), 1.1);
            SetInteger(ord(TProp.Phases), 1);
            SetDouble(ord(TProp.length), 0.001);
            changes := 5;
        end
        else
        begin
            if SymComponentsModel then
            begin  // keep the same Z1 and C1
                Z1.re := R1;
                Z1.im := X1;
                C1_new := C1 * 1.0e9; // convert to nF
            end
            else
            begin // matrix was input directly, or built from physical data
                // average the diagonal and off-dialgonal elements
                Zs := CZERO;
                for i := 1 to FnPhases do
                    Zs += Z.GetElement(i, i);
                Zs := Zs / (Fnphases * LengthMult);
                Zm := CZERO;
                for i := 1 to FnPhases - 1 do     // Corrected 6-21-04
                begin
                    for j := i + 1 to FnPhases do
                        Zm += Z.GetElement(i, j);
                end;
                Zm := Zm / (LengthMult * Fnphases * (FnPhases - 1.0) / 2.0);
                Z1 := Zs - Zm;

                // Do same for Capacitances
                Cs := 0.0;
                for i := 1 to FnPhases do
                    Cs := Cs + Yc.GetElement(i, i).im;
                Cm := 0.0;
                for i := 1 to FnPhases - 1 do    // corrected 4-9-2020
                    for j := i + 1 to FnPhases do
                        Cm := Cm + Yc.GetElement(i, j).im;
                C1_new := (Cs - Cm) / TwoPi / BaseFrequency / (LengthMult * Fnphases * (FnPhases - 1.0) / 2.0) * 1.0e9; // nanofarads

                // compensate for length units
                Z1 := Z1 / FunitsConvert;
                C1_New := C1_New / FunitsConvert;
            end;
            SetDouble(ord(TProp.R1), Z1.re);
            SetDouble(ord(TProp.X1), Z1.im);
            SetDouble(ord(TProp.C1), C1_new);
            SetInteger(ord(TProp.Phases), 1);
            changes := 4;
        end;
        // Conductor Current Ratings
        SetDouble(NumPropsThisClass + ord(TPDElementProp.NormAmps), NormAmps0);
        SetDouble(NumPropsThisClass + ord(TPDElementProp.EmergAmps), EmergAmps0);
        // Repeat the Length Units to compensate for unexpected reset
        SetInteger(ord(TProp.Units), LengthUnits0);
        EndEdit(changes + 3);
    end;
    inherited MakePosSequence();
end;

function TLineObj.MergeWith(var Other: TLineObj; Series: Boolean): Boolean; //TODO: remember to test this
// Merge this line with another line and disble the other line.
var
    Values1, Values2: pComplexArray;
    Order1, Order2,
    i, j,
    Common1, Common2: Integer;
    TotalLen: Double;
    NewName: String;
    TestBusNum: Integer;
    LenUnitsSaved: Integer;
    NewZ: Complex;
    LenSelf, LenOther: Double;
    RXC: Array[1..6] of Double;
    UseRXC: Boolean;
begin
    Result := FALSE;     // initialize the result
    if Other <> NIL then
    begin
        if Fnphases <> Other.Fnphases then
            Exit;  // Can't merge

        LenUnitsSaved := LengthUnits;

        YPrimInvalid := TRUE;

        // Redefine property values to make it appear that line was defined this way originally using matrices
        if Series then
            TotalLen := Len + Other.Len * ConvertLineUnits(Other.LengthUnits, LengthUnits)
        else
            TotalLen := 1.0;

        if Series then
        begin
            // redefine the bus connections

            // Find the bus in common between the two lines
            Common1 := 0;
            Common2 := 0;
            i := 1;
            while (Common1 = 0) and (i <= 2) do
            begin
                TestBusNum := ActiveCircuit.MapNodeToBus^[NodeRef^[1 + (i - 1) * Fnconds]].BusRef;
                for j := 1 to 2 do
                begin
                    if ActiveCircuit.MapNodeToBus^[Other.NodeRef^[1 + (j - 1) * Other.Nconds]].BusRef = TestBusNum then
                    begin
                        Common1 := i;
                        Common2 := j;
                        Break;
                    end;
                end;
                inc(i);
            end;

            if Common1 = 0 then
                Exit;  // There's been an error; didn't find anything in common

            // Redefine the bus connections, eliminating the common bus
            case Common1 of
                1:
                    case Common2 of
                        1:
                            ParsePropertyValue(ord(TProp.Bus1), Other.GetBus(2));
                        2:
                            ParsePropertyValue(ord(TProp.Bus1), Other.GetBus(1));
                    end;
                2:
                    case Common2 of
                        1:
                            ParsePropertyValue(ord(TProp.Bus2), Other.GetBus(2));
                        2:
                            ParsePropertyValue(ord(TProp.Bus2), Other.GetBus(1));
                    end;
            end;
        end; // If Series

        // Rename the line
        if Series then
            NewName := Other.Name + '~' + Name //(GetBus(1)) + '~'  + StripExtension(GetBus(2))
        else
            NewName := StripExtension(GetBus(1)) + '||' + StripExtension(GetBus(2));

        // Update ControlElement Connections to This Line 
        UpdateControlElements(self, Other);
        Name := NewName;

        if Series then
            IsSwitch := FALSE; // not allowed on series merge.

        // Now Do the impedances

        LenSelf := Len / FunitsConvert;  // in units of the R X Data
        LenOther := Other.Len / Other.FunitsConvert;

        // If both lines are Symmmetrical Components, just merge the R1..C0 values
        // This will catch many 3-phase lines since this is a common way to define lines
        if SymComponentsModel and Other.SymComponentsModel and (nphases = 3) then
        begin  // Sym Component Model
            UseRXC := False;
            // This reset the length units
            BeginEdit(True);
            if Series then
            begin // Ohms per unit length of this line length units
                RXC[1] := (R1 * LenSelf + Other.R1 * LenOther) / TotalLen; // R1
                RXC[2] := (X1 * LenSelf + Other.X1 * LenOther) / TotalLen; // X1
                RXC[3] := (R0 * LenSelf + Other.R0 * LenOther) / TotalLen; // R0
                RXC[4] := (X0 * LenSelf + Other.X0 * LenOther) / TotalLen; // X0
                RXC[5] := (C1 * LenSelf + Other.C1 * LenOther) / TotalLen * 1.0e9; // C1
                RXC[6] := (C0 * LenSelf + Other.C0 * LenOther) / TotalLen * 1.0e9; // C0
                UseRXC := True;
            end
            else // parallel
            begin
                if IsSwitch then
                begin 
                    // Leave as is if switch; just dummy z anyway
                end 
                else
                if Other.IsSwitch then
                    SetInteger(ord(TProp.Switch), 1)
                else
                begin
                    // ********* Will This work with Length multiplier?  did it ever work? *************************
                    NewZ := ParallelZ(Cmplx(R1 * Len, X1 * Len), Cmplx(Other.R1 * Other.Len, Other.X1 * Other.Len));
                    RXC[1] := NewZ.Re;
                    RXC[2] := NewZ.Im;
                    NewZ := ParallelZ(Cmplx(R0 * Len, X0 * Len), Cmplx(Other.R0 * Other.Len, Other.X0 * Other.Len));
                    RXC[3] := NewZ.Re;
                    RXC[4] := NewZ.Im;
                    RXC[3] := NewZ.Re;
                    RXC[4] := NewZ.Im;
                    RXC[5] := (C1 * Len + Other.C1 * Other.Len) / TotalLen * 1.0e9; // C1
                    RXC[6] := (C0 * Len + Other.C0 * Other.Len) / TotalLen * 1.0e9; // C0
                    UseRXC := True;
                end;
            end;
            if UseRXC then
            begin
                SetDouble(ord(TProp.R1), RXC[1]);
                SetDouble(ord(TProp.X1), RXC[2]);
                SetDouble(ord(TProp.R0), RXC[3]);
                SetDouble(ord(TProp.X0), RXC[4]);
                SetDouble(ord(TProp.C1), RXC[5]);
                SetDouble(ord(TProp.C0), RXC[6]);
            end;
            EndEdit(6);

            // update length units
            BeginEdit(False);
            SetDouble(ord(TProp.Length), TotalLen);
            SetInteger(ord(TProp.Units), LenUnitsSaved);
            EndEdit(2);
            // Update symmetrical Components computation
            // (Only time this function is called is for sym comp update -- computes Z and Yc)
            RecalcelementData;
        end
        else  //  Matrix Model for anything other than Symmetrical Components
        if not Series then
            // TODO: "fix" this?
            TotalLen := Len / 2.0 // We'll assume lines are equal for now 
        else
        begin  // Matrices were defined
            // Merge Z matrices
            Values1 := Z.GetValuesArrayPtr(Order1);
            Values2 := Other.Z.GetValuesArrayPtr(Order2);

            if Order1 <> Order2 then
                Exit;  // OOps.  Lines not same size for some reason

            // If Geometry specified, length is already included; so reset to 1.0
            if GeometrySpecified or SpacingSpecified then
                LenSelf := 1.0;
            if Other.GeometrySpecified or Other.SpacingSpecified then
                LenOther := 1.0;

            // Z <= (Z1 + Z2 )/TotalLen   to get equiv ohms per unit length
            for i := 1 to Order1 * Order1 do
                Values1[i] := (Values1[i] * LenSelf + Values2^[i] * LenOther) / TotalLen;

            // Merge Yc matrices
            Values1 := Yc.GetValuesArrayPtr(Order1);
            Values2 := Other.Yc.GetValuesArrayPtr(Order2);

            if Order1 <> Order2 then
                Exit;  // OOps.  Lines not same size for some reason

            for i := 1 to Order1 * Order1 do
                Values1[i] := (Values1[i] * LenSelf + Values2[i] * LenOther) / TotalLen;

            Len := TotalLen;
            LengthUnits := LenUnitsSaved;

            // Just mark the properties as updated
            SetAsNextSeq(ord(TProp.rmatrix));
            SetAsNextSeq(ord(TProp.xmatrix));
            SetAsNextSeq(ord(TProp.cmatrix));
            SetAsNextSeq(ord(TProp.length));
            SetAsNextSeq(ord(TProp.units));
            PropertySideEffects(ord(TProp.rmatrix));
            PropertySideEffects(ord(TProp.xmatrix));
            PropertySideEffects(ord(TProp.cmatrix));
        end;  // Matrix definition

        Other.Enabled := FALSE;  // Disable the Other Line
        Result := TRUE;
    end
    else
        DoSimpleMsg(_('Error in Line Merge: Attempt to merge with invalid (nil) line object found.'), 184);
end;

procedure TLineObj.UpdateControlElements(NewLine, OldLine: TLineObj);
var
    pControlElem: TControlElem;
begin
    pControlElem := ActiveCircuit.DSSControls.First;
    while pControlElem <> NIL do
    begin
        if OldLine = pControlElem.MonitoredElement then // TODO: check if this works (and needs to work) with Fuse
            pControlElem.ParsePropertyValue(pControlElem.ParentClass.CommandList.GetCommand('element'), NewLine.FullName);
        pControlElem := ActiveCircuit.DSSControls.Next;
    end;
end;

procedure TLineObj.FetchLineSpacing();
begin
    if LineSpacingObj = NIL then
        Exit;

    LineCodeObj := NIL;
    KillGeometrySpecified;
    // need to establish Yorder before FMakeZFromSpacing
    FNPhases := LineSpacingObj.NPhases;
    Nconds := FNPhases;  // Force Reallocation of terminal info
    Yorder := Fnconds * Fnterms;
    YPrimInvalid := TRUE;       // Force Rebuild of Y matrix

    FLineWireData := Allocmem(Sizeof(FLineWireData^[1]) * LineSpacingObj.NWires);
    FWireDataSize := LineSpacingObj.NWires;
end;

procedure TLineObj.FetchGeometryCode();
var
    i: Integer;
begin
    if LineGeometryObj = NIL then
        Exit;

    LineCodeObj := NIL;
    KillSpacingSpecified();

    FZFrequency := -1.0;  // Init to signify not computed

    if FrhoSpecified then
        LineGeometryObj.rhoearth := rho;

    NormAmps := LineGeometryObj.NormAmps;
    EmergAmps := LineGeometryObj.EmergAmps;
    SetAsNextSeq(ord(TProp.Ratings));

    FNPhases := LineGeometryObj.Nconds;
    Nconds := FNPhases;  // Force Reallocation of terminal info
    Yorder := Fnconds * Fnterms;
    YPrimInvalid := TRUE;       // Force Rebuild of Y matrix

    NumAmpRatings := LineGeometryObj.NumAmpRatings;
    setlength(AmpRatings, NumAmpRatings);
    for i := 0 to High(AmpRatings) do
        AmpRatings[i] := LineGeometryObj.AmpRatings[i];

    FLineType := LineGeometryObj.FLineType;

    SymComponentsModel := FALSE;
    SymComponentsChanged := FALSE;
end;

procedure TLineObj.FMakeZFromGeometry(f: Double); // make new Z, Zinv, Yc, etc
begin
    if f = FZFrequency then
        exit;  // Already Done for this frequency, no need to do anything

    if LineGeometryObj = NIL then
        Exit;

    // This will make a New Z; Throw away present allocations

    if assigned(Z) then
        FreeAndNil(Z);
    if assigned(Zinv) then
        FreeAndNil(Zinv);

    if assigned(Yc) then
        FreeAndNil(Yc);

    DSS.ActiveEarthModel := FEarthModel;

    Z := LineGeometryObj.Zmatrix[f, len, LengthUnits];
    Yc := LineGeometryObj.YCmatrix[f, len, LengthUnits];
    // Init Zinv
    if Assigned(Z) then
    begin
        Zinv := TCMatrix.CreateMatrix(Z.order);  // Either no. phases or no. conductors
        Zinv.CopyFrom(Z);
        Zinv.Invert;  // Invert Z in place to get values to put in Yprim
    end;

    // Z and YC are actual total impedance for the line;

    FZFrequency := f;
end;

procedure TLineObj.FMakeZFromSpacing(f: Double); // make new Z, Zinv, Yc, etc
var
    pGeo: TLineGeometryObj;
begin
    if f = FZFrequency then
        exit;  // Already Done for this frequency, no need to do anything

    if assigned(Z) then
    begin
        Z.Free;
        Z := NIL;
    end;
    if assigned(Zinv) then
    begin
        Zinv.Free;
        Zinv := NIL;
    end;
    if assigned(Yc) then
    begin
        Yc.Free;
        Yc := NIL;
    end;

    // make a temporary LineGeometry to calculate line constants
    pGeo := TLineGeometryObj.Create(DSS.LineGeometryClass, Name);
    pGeo.LoadSpacingAndWires(LineSpacingObj, FLineWireData); // this sets OH, CN, or TS

    if FrhoSpecified then
        pGeo.rhoearth := rho;
    NormAmps := pGeo.NormAmps;
    EmergAmps := pGeo.EmergAmps;
    SetAsNextSeq(ord(TProp.Ratings));

    DSS.ActiveEarthModel := FEarthModel;

    Z := pGeo.Zmatrix[f, len, LengthUnits];
    Yc := pGeo.YCmatrix[f, len, LengthUnits];
    if Assigned(Z) then
    begin
        Zinv := TCMatrix.CreateMatrix(Z.order);  // Either no. phases or no. conductors
        Zinv.CopyFrom(Z);
        Zinv.Invert;  // Invert Z in place to get values to put in Yprim
    end;
    pGeo.Free;

    FZFrequency := f;
end;

procedure TLineObj.KillGeometrySpecified;
begin
    // Indicate No Line Geometry specification if this is called
    if GeometrySpecified then
    begin
        LineGeometryObj := NIL;
        FZFrequency := -1.0;
    end;
end;

procedure TLineObj.KillSpacingSpecified;
begin
    if SpacingSpecified then
    begin
        LineSpacingObj := NIL;
        FWireDataSize := 0;
        Reallocmem(FLineWireData, 0);
        FPhaseChoice := Unknown;
        FZFrequency := -1.0;
    end;
end;

procedure TLineObj.ClearYPrim;
begin
    // Line Object needs both Series and Shunt YPrims built
    if (Yprim = NIL) OR (Yprim.order <> Yorder) OR (Yprim_Shunt = NIL) OR (Yprim_Series = NIL) {YPrimInvalid} then
    begin // Reallocate YPrim if something has invalidated old allocation
        if YPrim_Series <> NIL then
            YPrim_Series.Free;
        if YPrim_Shunt <> NIL then
            YPrim_Shunt.Free;
        if YPrim <> NIL then
            YPrim.Free;

        YPrim_Series := TcMatrix.CreateMatrix(Yorder);
        YPrim_Shunt := TcMatrix.CreateMatrix(Yorder);
        YPrim := TcMatrix.CreateMatrix(Yorder);
    end
    else
    begin
        YPrim_Series.Clear;   // zero out YPrim Series
        YPrim_Shunt.Clear;    // zero out YPrim Shunt
        YPrim.Clear;          // zero out YPrim
    end;
end;

procedure TLineObj.ConvertZinvToPosSeqR;
// For GIC Analysis, use only real part of Z
var
    Z1, ZS, Zm: Complex;
    i: Integer;
begin
    // re-invert Zinv
    Zinv.Invert;
    // Now Zinv is back to Z with length included

    // average the diagonal and off-dialgonal elements
    Zs := Zinv.AvgDiagonal;
    Zm := Zinv.AvgOffDiagonal;
    Z1 := Zs - Zm;
    Z1.im := 0.0;  // ignore X part

    Zinv.Clear;
    for i := 1 to Zinv.order do
        Zinv.SetElement(i, i, Z1);   // Set Diagonals

    Zinv.Invert;  // back to zinv for inserting in Yprim

end;

procedure TLineObj.ResetLengthUnits;
// If specify the impedances always assume the length units match
begin
    FUnitsConvert := 1.0;
    LengthUnits := UNITS_NONE; // but do not erase FUserLengthUnits, in case of CIM export
end;

function TLineObj.NumConductorData: Integer;
begin
    Result := 0;
    if Assigned(FLineWireData) then
        Result := LineSpacingObj.NWires;
    if Assigned(LineGeometryObj) then
        Result := LineGeometryObj.NWires;
end;

function TLineObj.FetchConductorData(i: Integer): TConductorDataObj;
begin
    Result := NIL;
    if Assigned(FLineWireData) then
    begin
        if i <= LineSpacingObj.Nwires then
            Result := FLineWireData[i];
    end
    else
    if Assigned(LineGeometryObj) then
    begin
        if i <= LineGeometryObj.Nwires then
            Result := LineGeometryObj.ConductorData[i];
    end;
end;

function TLineObj.LineCodeSpecified: Boolean;
begin
    Result := LineCodeObj <> NIL;
end;

function TLineObj.GeometrySpecified: Boolean;
begin
    Result := LineGeometryObj <> NIL;
end;

function TLineObj.SpacingSpecified: Boolean;
begin
    Result := Assigned(LineSpacingObj) and Assigned(FLineWireData);
end;

end.