unit LineGeometry;

// ----------------------------------------------------------
// Copyright (c) 2008-2015, Electric Power Research Institute, Inc.
// All rights reserved.
// ----------------------------------------------------------

interface

// The LineGeometry object is a general DSS object used by all circuits
// as a reference for obtaining line impedances.
//
// The values are set by the normal New and Edit procedures for any DSS object.
//
// The values are retrieved by setting the Code Property in the LineGeometry Class.
// This sets the active LineGeometry object to be the one referenced by the Code Property;
//
// Then the values of that code can be retrieved via the public variables.

uses
    Classes,
    Sysutils,
    Arraydef,
    Command,
    DSSClass,
    DSSObject,
    uCMatrix,
    LineConstants,
    ConductorData,
    CNData,
    TSData,
    LineSpacing;

type
{$SCOPEDENUMS ON}
    TLineGeometryPropLegacy = (
        INVALID = 0,
        nconds = 1,
        nphases = 2,
        cond = 3,
        wire = 4,
        x = 5,
        h = 6,
        units = 7,
        normamps = 8,
        emergamps = 9,
        reduce = 10,
        spacing = 11,
        wires = 12,
        cncable = 13,
        tscable = 14,
        cncables = 15,
        tscables = 16,
        Seasons = 17,
        Ratings = 18,
        LineType = 19,
        conductors = 20
    );
    TLineGeometryProp = (
        INVALID = 0,
        NConds = 1,
        NPhases = 2,
        Cond = 3,
        Wire = 4,
        X = 5,
        H = 6,
        Units = 7,
        NormAmps = 8,
        EmergAmps = 9,
        Reduce = 10,
        Spacing = 11,
        Wires = 12,
        CNCable = 13,
        TSCable = 14,
        CNCables = 15,
        TSCables = 16,
        Seasons = 17,
        Ratings = 18,
        LineType = 19,
        Conductors = 20
    );
{$SCOPEDENUMS OFF}

    ELineGeometryProblem = class(Exception);

    TLineGeometry = class(TDSSClass)
    PROTECTED
        procedure DefineProperties(); override;
    PUBLIC
        ConductorProxyClass: TProxyClass;
        
        constructor Create(dssContext: TDSSContext);
        destructor Destroy; OVERRIDE;
        Function NewObject(const ObjName: String; Activate: Boolean = True): Pointer; OVERRIDE;
    end;

    TLineGeometryObj = class(TDSSObject)
    PUBLIC
        phaseChoice: pConductorChoiceArray; // TODO: remove -- somewhat redundant with conductors (FWireData)
        FNConds: Integer;
        FNPhases: Integer;
        conductors: pConductorDataArray; // was originally FWireData
        xCoord: pDoubleArray;
        yCoord: pDoubleArray;
        eqDistPhPh, eqDistPhN, avgPhaseHeight, avgNeutralHeight: Double;
        equivalentSpacing: Boolean;  // to tell the calcs when to use equivalent spacing info
        
        units: pIntegerArray;
        FLastUnit: Integer;
        dataChanged: Boolean;
        FReduce: LongBool;
        FActiveCond, firstValidCond: Integer;

        lineConstants: TLineConstants;

        NormAmps: Double;
        EmergAmps: Double;
        NumAmpRatings: Integer;
        AmpRatings: array of Double;
        FLineType: Integer; // Pointer to code for type of line
        lineSpacingObj: TLineSpacingObj;

        procedure ChangeLineConstantsType(newPhaseChoice: ConductorChoice);

        procedure SetNPhases(const Value: Integer);
        procedure SetActiveCond(const Value: Integer);

        function NConds(): Integer;
        procedure SetNConds(const Value: Integer);

        procedure UpdateLineGeometryData(f: Double; earthModel: Integer);   // call this before using the line data

        constructor Create(ParClass: TDSSClass; const LineGeometryName: String);
        destructor Destroy; OVERRIDE;
        procedure PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags); override;
        procedure MakeLike(OtherPtr: Pointer); override;
        
        procedure DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean = False); OVERRIDE;
        procedure SaveWrite(F: TStream); OVERRIDE;

        // called from a Line object that has its own Spacing and Wires input
        // automatically sets reduce=y if the spacing has more wires than phases
        procedure LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray; earthModel: Integer);
        function GetYCMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;
        function GetZMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;        
    end;

implementation

uses
    DSSClassDefs,
    DSSGlobals,
    UComplex, DSSUcomplex,
    Utilities,
    LineUnits,
    WireData,
    OHLineConstants,
    CableConstants,
    Math,
    DSSHelper,
    DSSObjectHelper,
    TypInfo;

type
    TObj = TLineGeometryObj;
    TProp = TLineGeometryProp;
    TPropLegacy = TLineGeometryPropLegacy;
const
    NumPropsThisClass = Ord(High(TProp));
var
    PropInfo: Pointer = NIL;
    PropInfoLegacy: Pointer = NIL;    

constructor TLineGeometry.Create(dssContext: TDSSContext);
begin
    if PropInfo = NIL then
    begin
        PropInfo := TypeInfo(TProp);
        PropInfoLegacy := TypeInfo(TPropLegacy);
    end;

    ConductorProxyClass := TProxyClass.Create(dssContext, ['WireData', 'CNData', 'TSData'], True);
    ConductorProxyClass.Name := 'Conductor';

    inherited Create(dssContext, DSS_OBJECT, 'LineGeometry');
    RequiresCircuit := true;
end;

destructor TLineGeometry.Destroy;
begin
    ConductorProxyClass.Free;
    inherited Destroy;
end;

procedure SetWires(obj: TObj; Value: TDSSObjectPtr; ValueCount: Integer; setterFlags: TDSSPropertySetterFlags);
var
    i, istart, istop: Integer;
begin
    istart := 1;
    istop := obj.FNConds;

    if obj.phaseChoice[obj.FActiveCond] = Unknown then
        obj.ChangeLineConstantsType(Overhead)
    else if obj.phaseChoice[obj.FActiveCond] <> Overhead then
        // these are buried neutral wires 
        // (only when the phase conductors not overhead)
        istart := obj.FNPhases + 1;

    // Validate number of elements
    if (istop - istart + 1) <> ValueCount then
    begin
        obj.DoSimpleMsg('%s: Unexpected number (%d) of objects; expected %d objects.', 
            [obj.FullName(), ValueCount, (istop - istart + 1)], 18102);
        Exit;
    end;

    for i := istart to istop do
    begin
        obj.conductors[i] := TConductorDataObj(Value^);
        Inc(Value);
    end;
    obj.FActiveCond := istop;
end;

procedure TLineGeometry.DefineProperties();
var 
    obj: TObj = NIL; // NIL (0) on purpose
begin
    Numproperties := NumPropsThisClass;
    CountPropertiesAndAllocate();
    PopulatePropertyNames(0, NumPropsThisClass, PropInfo, PropInfoLegacy);

    SpecSetNames := ArrayOfString.Create(
        'LineSpacing',
        'x, h'
    );
    SpecSets := TSpecSets.Create(
        TSpecSet.Create(ord(TProp.spacing)),
        TSpecSet.Create(ord(TProp.x), ord(TProp.h))
    );

    PropertyStructArrayCountOffset := ptruint(@obj.FNConds);
    PropertyStructArrayIndexOffset := ptruint(@obj.FActiveCond);
    PropertyStructArrayIndexOffset2 := ptruint(@obj.FNPhases);

    // list of objects
    PropertyType[ord(TProp.TSCables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.TSCables)] := ptruint(@obj.conductors);
    PropertyOffset2[ord(TProp.TSCables)] := ptruint(DSS.TSDataClass);
    PropertyFlags[ord(TProp.TSCables)] := [TPropertyFlag.AltIndex, TPropertyFlag.Redundant, TPropertyFlag.SuppressJSON, TPropertyFlag.AllowNoneItem];
    PropertyRedundantWith[ord(TProp.TSCables)] := ord(TProp.TSCable);
    PropertyArrayAlternative[ord(TProp.TSCable)] := ord(TProp.TSCables);
    
    PropertyType[ord(TProp.CNCables)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.CNCables)] := ptruint(@obj.conductors);
    PropertyOffset2[ord(TProp.CNCables)] := ptruint(DSS.CNDataClass);
    PropertyFlags[ord(TProp.CNCables)] := [TPropertyFlag.AltIndex, TPropertyFlag.Redundant, TPropertyFlag.SuppressJSON, TPropertyFlag.AllowNoneItem];
    PropertyRedundantWith[ord(TProp.CNCables)] := ord(TProp.CNCable);
    PropertyArrayAlternative[ord(TProp.CNCable)] := ord(TProp.CNCables);

    PropertyType[ord(TProp.Wires)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.Wires)] := ptruint(@obj.conductors);
    PropertyOffset2[ord(TProp.Wires)] := ptruint(DSS.WireDataClass);
    PropertyOffset3[ord(TProp.Wires)] := ptruint(@obj.FNConds);
    PropertyWriteFunction[ord(TProp.Wires)] := @SetWires;
    PropertyFlags[ord(TProp.Wires)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.Redundant, TPropertyFlag.SuppressJSON, TPropertyFlag.AllowNoneItem];
    // PropertyFlags[ord(TProp.Wires)] := [TPropertyFlag.WriteByFunction, TPropertyFlag.FullNameAsArray, TPropertyFlag.FullNameAsJSONArray];
    PropertyRedundantWith[ord(TProp.Wires)] := ord(TProp.Conductors);
    // PropertyNameJSON[ord(TProp.Wires)] := 'Conductors';

    PropertyArrayAlternative[ord(TProp.Wire)] := ord(TProp.Wires);

    PropertyType[ord(TProp.Conductors)] := TPropertyType.DSSObjectReferenceArrayProperty;
    PropertyOffset[ord(TProp.Conductors)] := ptruint(@obj.conductors);
    PropertyOffset2[ord(TProp.Conductors)] := ptruint(ConductorProxyClass);
    PropertyOffset3[ord(TProp.Conductors)] := ptruint(@obj.FNConds);
    PropertyFlags[ord(TProp.Conductors)] := [TPropertyFlag.FullNameAsArray, TPropertyFlag.FullNameAsJSONArray, TPropertyFlag.AllowNoneItem];

    // enums
    PropertyType[ord(TProp.units)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.units)] := ptruint(@obj.units);
    PropertyOffset2[ord(TProp.units)] := PtrInt(DSS.UnitsEnum);
    PropertyFlags[ord(TProp.units)] := [TPropertyFlag.OnArray];

    PropertyType[ord(TProp.linetype)] := TPropertyType.MappedStringEnumProperty;
    PropertyOffset[ord(TProp.linetype)] := ptruint(@obj.FLineType);
    PropertyOffset2[ord(TProp.linetype)] := PtrInt(DSS.LineTypeEnum);

    // object properties
    PropertyType[ord(TProp.spacing)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyOffset[ord(TProp.spacing)] := ptruint(@obj.lineSpacingObj);
    PropertyOffset2[ord(TProp.spacing)] := ptruint(DSS.LineSpacingClass);
    PropertyFlags[ord(TProp.spacing)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.Ratings)] := TPropertyType.DoubleDArrayProperty;
    PropertyOffset[ord(TProp.Ratings)] := ptruint(@obj.AmpRatings);
    PropertyOffset2[ord(TProp.Ratings)] := ptruint(@obj.NumAmpRatings);

    PropertyType[ord(TProp.reduce)] := TPropertyType.BooleanProperty;
    PropertyOffset[ord(TProp.reduce)] := ptruint(@obj.Freduce);

    PropertyType[ord(TProp.NPhases)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.NConds)] := TPropertyType.IntegerProperty;
    PropertyType[ord(TProp.cond)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.NPhases)] := ptruint(@obj.FNPhases);
    PropertyOffset[ord(TProp.NConds)] := ptruint(@obj.FNConds);
    PropertyOffset[ord(TProp.cond)] := ptruint(@obj.FActiveCond);
    PropertyFlags[ord(TProp.NPhases)] := [TPropertyFlag.NonNegative]; // phases can be zero (e.g. only neutral cables)
    PropertyFlags[ord(TProp.NConds)] := [TPropertyFlag.NonNegative, TPropertyFlag.NonZero];
    PropertyFlags[ord(TProp.cond)] := [TPropertyFlag.IntegerStructIndex];

    PropertyType[ord(TProp.Seasons)] := TPropertyType.IntegerProperty;
    PropertyOffset[ord(TProp.Seasons)] := ptruint(@obj.NumAmpRatings);
    PropertyFlags[ord(TProp.Seasons)] := [TPropertyFlag.SuppressJSON]; // can be derived trivially from length(Ratings)

    PropertyType[ord(TProp.Wire)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.CNCable)] := TPropertyType.DSSObjectReferenceProperty;
    PropertyType[ord(TProp.TSCable)] := TPropertyType.DSSObjectReferenceProperty;

    PropertyOffset[ord(TProp.Wire)] := ptruint(@obj.conductors);
    PropertyOffset[ord(TProp.CNCable)] := ptruint(@obj.conductors);
    PropertyOffset[ord(TProp.TSCable)] := ptruint(@obj.conductors);
    
    PropertyOffset2[ord(TProp.Wire)] := ptruint(DSS.WireDataClass);
    PropertyOffset2[ord(TProp.CNCable)] := ptruint(DSS.CNDataClass);
    PropertyOffset2[ord(TProp.TSCable)] := ptruint(DSS.TSDataClass);

    PropertyFlags[ord(TProp.Wire)] := [TPropertyFlag.Redundant, TPropertyFlag.OnArray, TPropertyFlag.FullNameAsArray];
    PropertyFlags[ord(TProp.CNCable)] := [TPropertyFlag.Redundant, TPropertyFlag.OnArray, TPropertyFlag.SuppressJSON];
    PropertyFlags[ord(TProp.TSCable)] := [TPropertyFlag.Redundant, TPropertyFlag.OnArray, TPropertyFlag.SuppressJSON];
    PropertyRedundantWith[ord(TProp.CNCable)] := ord(TProp.Conductors);
    PropertyRedundantWith[ord(TProp.TSCable)] := ord(TProp.Conductors);
    PropertyRedundantWith[ord(TProp.Wire)] := ord(TProp.Conductors);

    PropertyType[ord(TProp.x)] := TPropertyType.DoubleOnArrayProperty; //TODO: use TPropertyFlag.OnArray instead
    PropertyOffset[ord(TProp.x)] := ptruint(@obj.xCoord); 
    PropertyOffset2[ord(TProp.x)] := ptruint(@obj.FActiveCond);
    PropertyFlags[ord(TProp.x)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyType[ord(TProp.h)] := TPropertyType.DoubleOnArrayProperty; //TODO: use TPropertyFlag.OnArray instead
    PropertyOffset[ord(TProp.h)] := ptruint(@obj.yCoord); 
    PropertyOffset2[ord(TProp.h)] := ptruint(@obj.FActiveCond);
    PropertyFlags[ord(TProp.h)] := [TPropertyFlag.RequiredInSpecSet];

    PropertyOffset[ord(TProp.NormAmps)] := ptruint(@obj.NormAmps);
    PropertyOffset[ord(TProp.EmergAmps)] := ptruint(@obj.EmergAmps);

    ActiveProperty := NumPropsThisClass;
    inherited DefineProperties();
end;

function TLineGeometry.NewObject(const ObjName: String; Activate: Boolean): Pointer;
var
    obj: TObj;
begin
    obj := TObj.Create(Self, ObjName);
    if Activate then 
        DSS.ActiveDSSObject := obj;
    obj.ClassIndex := AddObjectToList(obj, Activate);
    Result := obj;
end;

procedure TLineGeometryObj.PropertySideEffects(Idx: Integer; previousIntVal: Integer; setterFlags: TDSSPropertySetterFlags);
var
    tmpName: String;
    i: Integer;
    conductorObj: TConductorDataObj = NIL;
    anyConductor: Boolean;
begin
    case Idx of
        ord(TProp.NPhases):
            if lineConstants <> NIL then
            begin
                lineConstants.Nphases := FNPhases;
                if (lineConstants.Nphases > FNConds) then
                    lineConstants.Nphases := FNConds;
            end;
        ord(TProp.cond):
            if units[FactiveCond] = -1 then
                units[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
        ord(TProp.Wire):
            if phaseChoice[FActiveCond] = Unknown then
                ChangeLineConstantsType(Overhead);
        ord(TProp.units):
            FLastUnit := units[FActiveCond];
        ord(TProp.CNCable), ord(TProp.CNCables):
            ChangeLineConstantsType(ConcentricNeutral);
        ord(TProp.TSCable), ord(TProp.TSCables):
            ChangeLineConstantsType(TapeShield);
        ord(TProp.NConds):
        begin
            if previousIntVal <> FNConds then
            begin
                if Assigned(lineConstants) then
                    FreeAndNil(lineConstants);

                // Allocations
                Reallocmem(conductors, Sizeof(conductors[1]) * FNConds);
                for i := max(1, previousIntVal) to FNConds do
                    conductors[i] := NIL;

                Reallocmem(xCoord, Sizeof(Double) * FNConds);
                Reallocmem(yCoord, Sizeof(Double) * FNConds);
                Reallocmem(units, Sizeof(units[1]) * FNConds);
                Reallocmem(phaseChoice, Sizeof(phaseChoice[1]) * FNConds);
            end
            else
            begin
                for i := 1 to FNConds do
                    conductors[i] := NIL;
            end;

            // For compatibility with the official version, always zero the eq dist values
            eqDistPhPh := 0;
            eqDistPhN := 0;
            avgPhaseHeight := 0;
            avgNeutralHeight := 0;

            if FNConds > previousIntVal then
                for i := Max(1, previousIntVal) to FNConds do
                    phaseChoice[i] := Unknown;

            for i := 1 to FNConds do
            begin
                FActiveCond := i;
                ChangeLineConstantsType(Overhead); // works on activecond
            end;
            // Reset the active conductor
            FActiveCond := 1;
            firstValidCond := FNConds + 1;

            // Initialize Allocations
            for i := 1 to FNConds do
                phaseChoice[i] := Overhead;
            for i := 1 to FNConds do
                conductors[i] := NIL;
            for i := 1 to FNConds do
                xCoord[i] := 0.0;
            for i := 1 to FNConds do
                yCoord[i] := 0.0;
            for i := 1 to FNConds do
                units[i] := -1;  // default to ft
            FLastUnit := UNITS_FT;
        end;
        ord(TProp.spacing):
            if lineSpacingObj <> NIL then
            begin
                if (FNConds = lineSpacingObj.NConds) then
                begin
                    FLastUnit := lineSpacingObj.Units;
                    equivalentSpacing := lineSpacingObj.EquivalentSpacing();
                    if equivalentSpacing then
                    begin
                        eqDistPhPh := lineSpacingObj.eqDistPhPh;
                        eqDistPhN := lineSpacingObj.eqDistPhN;
                        avgPhaseHeight := lineSpacingObj.avgPhaseHeight;
                        avgNeutralHeight := lineSpacingObj.avgNeutralHeight;
                    end
                    else                    
                    begin
                        for i := 1 to FNConds do
                        begin
                            xCoord[i] := lineSpacingObj.GetXCoord(i);
                            yCoord[i] := lineSpacingObj.GetYCoord(i);
                            units[i] := FLastUnit;
                        end;
                    end;
                    if (DSS_EXTENSIONS_COMPAT and ord(DSSCompatFlag.NoPropertyTracking)) = 0 then
                    begin
                        PrpSequence[ord(TProp.X)] := 0;
                        PrpSequence[ord(TProp.H)] := 0;
                    end;
                end
                else
                begin
                    DoSimpleMsg('LineSpacing object %s has the wrong number of wires.', [lineSpacingObj.Name], 10103);
                end;
            end;
    end;

    //TODO: handle property tracking and ratings in the following block

    case Idx of 
        ord(TProp.Conductors):
        begin
            // Special handling for "Conductors"

            // Simulate setting the conductors one by one
            // Much easier/safer than reproducing the whole code paths
            anyConductor := false;
            for i := 1 to FNConds do
            begin
                SetActiveCond(i);
                if (Idx = ord(TProp.Conductors)) and (conductors[FActiveCond] = NIL) then
                begin
                    // The new "Conductors" property (after OpenDSS 10.1) accepts empty conductors; we just skip them.
                    continue;
                end;

                if not anyConductor then
                begin
                    anyConductor := true;
                    firstValidCond := i;
                end;

                if conductors[FActiveCond] is TWireDataObj then 
                begin
                    PropertySideEffects(ord(TProp.Wire), 0, setterFlags);
                    continue;
                end;
                if conductors[FActiveCond] is TCNDataObj then 
                begin
                    PropertySideEffects(ord(TProp.CNCable), 0, setterFlags);
                    continue;
                end;
                if conductors[FActiveCond] is TTSDataObj then 
                begin
                    PropertySideEffects(ord(TProp.TSCable), 0, setterFlags);
                    continue;
                end;
            end;
            if not anyConductor then
            begin
                DoSimpleMsg('%s.%s: At least one valid conductor must be provided.', [FullName(), ParentClass.PropertyName[Idx]], 10103);
                Exit;
            end;
        end;

        ord(TProp.Wires),
        ord(TProp.CNCables),
        ord(TProp.TSCables):
        begin
            // Traditional wires/cncables/tscables            
            i := 1;
            if Idx = ord(TProp.Wires) then
            begin
                if phaseChoice[FActiveCond] = Unknown then
                begin
                    // no other cables set for ActiveCond
                end
                else 
                if phaseChoice[FActiveCond] <> Overhead then
                begin
                    // these are buried neutral wires
                    // (only when the phase conductors not overhead)
                    i := FNPhases + 1;
                end;
            end;
            if i = 1 then
            begin
                conductorObj := conductors[1];
                if (conductorObj.NormAmps > 0.0) and (Normamps = 0.0) then 
                    Normamps  := conductorObj.NormAmps;
                
                if (conductorObj.Emergamps > 0.0) and (Emergamps = 0.0) then 
                    Emergamps := conductorObj.EmergAmps;
                
                if (conductorObj.NumAmpRatings > 1) and (NumAmpRatings = 1) then 
                    NumAmpRatings  := conductorObj.NumAmpRatings;

                if (Length(conductorObj.AmpRatings) > 1) and (length(AmpRatings) = 1) then
                begin
                    AmpRatings := Copy(conductorObj.AmpRatings, 0, Length(conductorObj.AmpRatings));
                end;
            end;
        end;
        ord(TProp.Wire), ord(TProp.CNCable), ord(TProp.TSCable):
        begin
            conductorObj := conductors[FActiveCond];
            if conductorObj <> nil then // NIL should be handled by the parser system already; no need to produce errors here
            begin
                if FActiveCond < firstValidCond then
                begin
                    firstValidCond := FActiveCond;
                end;

                // conductors[ActiveCond] := conductorObj;
                // Default the current ratings for this geometry to the rating of the first conductor
                if (FActiveCond = firstValidCond) then
                begin
                    if (conductorObj.NormAmps > 0.0) and (Normamps = 0.0) then
                        Normamps := conductorObj.NormAmps;
                    if (conductorObj.Emergamps > 0.0) and (Emergamps = 0.0) then
                        Emergamps := conductorObj.EmergAmps;
                    if (conductorObj.NumAmpRatings > 1) and (NumAmpRatings = 1) then
                        NumAmpRatings := conductorObj.NumAmpRatings;
                    if (length(conductorObj.AmpRatings) > 1) and (length(AmpRatings) = 1) then
                    begin
                        SetLength(AmpRatings, NumAmpRatings);
                        AmpRatings := Copy(conductorObj.AmpRatings, 0, Min(Length(conductorObj.AmpRatings), NumAmpRatings));
                    end;
                end;
            end;
        end;
        ord(TProp.Seasons):
            setlength(AmpRatings, NumAmpRatings);
    end;

    case Idx of
        ord(TProp.NConds),
        ord(TProp.Wire),
        ord(TProp.X),
        ord(TProp.H),
        ord(TProp.Units),
        ord(TProp.Spacing),
        ord(TProp.Wires),
        ord(TProp.CNCable),
        ord(TProp.TSCable),
        ord(TProp.CNCables),
        ord(TProp.TSCables),
        ord(TProp.Conductors):
            dataChanged := TRUE;
    end;

    inherited PropertySideEffects(Idx, previousIntVal, setterFlags);
end;

procedure TLineGeometryObj.MakeLike(OtherPtr: Pointer);
var
    Other: TObj;
    i: Integer;
begin
    inherited MakeLike(OtherPtr);
    Other := TObj(OtherPtr);
    SetNConds(Other.FNConds); // allocates
    FNPhases := Other.FNPhases;

    lineSpacingObj := Other.lineSpacingObj;
    eqDistPhPh := Other.eqDistPhPh;
    eqDistPhN := Other.eqDistPhN;
    avgPhaseHeight := Other.avgPhaseHeight;
    avgNeutralHeight := Other.avgNeutralHeight;
    equivalentSpacing := Other.equivalentSpacing;

    FLineType := Other.FLineType;
    for i := 1 to FNConds do
        phaseChoice[i] := Other.phaseChoice[i];
    for i := 1 to FNConds do
        conductors[i] := Other.conductors[i];
    for i := 1 to FNConds do
        xCoord[i] := Other.xCoord[i];
    for i := 1 to FNConds do
        yCoord[i] := Other.yCoord[i];
    for i := 1 to FNConds do
        units[i] := Other.units[i];

    FLastUnit := Other.FLastUnit; // Useful if template geometry uses a spacing
    dataChanged := TRUE;
    NormAmps := Other.NormAmps;
    EmergAmps := Other.EmergAmps;
    FReduce := Other.FReduce;

    UpdateLineGeometryData(activecircuit.Solution.Frequency(), DSS.ActiveEarthModel);
end;

constructor TLineGeometryObj.Create(ParClass: TDSSClass; const LineGeometryName: String);
begin
    inherited Create(ParClass, LineGeometryName);
    DSSObjType := ParClass.DSSClassType;

    dataChanged := TRUE;

    phaseChoice := NIL;
    conductors := NIL;
    xCoord := NIL;
    yCoord := NIL;
    units := NIL;
    lineConstants := NIL;
    lineSpacingObj := NIL;
    equivalentSpacing := False;
    eqDistPhPh := NaN;
    eqDistPhN := NaN;
    avgPhaseHeight := NaN;
    avgNeutralHeight := NaN;

    // was causing unnecessary allocations (was leaving dangling memory)
    // Nconds      := 3;  // Allocates terminals
    // FNPhases    := 3;

    FNConds := 0;
    FNPhases := 0;
    // SetActiveCond(1);
    FActiveCond := 1;
    FLastUnit := UNITS_FT;
    Normamps := 0.0;
    EmergAmps := 0.0;
    FLineType := 1;  // Default to OH Line

    FReduce := FALSE;
    NumAmpRatings := 1;
    setlength(AmpRatings, NumAmpRatings);
    AmpRatings[0] := NormAmps;
end;

destructor TLineGeometryObj.Destroy;
begin
    if lineConstants <> NIL then
        lineConstants.Free;
    Reallocmem(conductors, 0);
    Reallocmem(yCoord, 0);
    Reallocmem(xCoord, 0);
    Reallocmem(units, 0);
    Reallocmem(phaseChoice, 0);

    inherited destroy;
end;

procedure TLineGeometryObj.DumpProperties(F: TStream; Complete: Boolean; Leaf: Boolean);
var
    i, j: Integer;
begin
    FSWriteln(F, '! WARNING: when mixing wire/cable types, "wires", "cncables" and "tscables" may not make sense in this dump');

    inherited DumpProperties(F, Complete);

    for i := 1 to 2 do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue(i));
    end;
    for j := 1 to FNConds do
    begin
        SetActiveCond(j);
        FSWriteln(F, '~ ' + ParentClass.PropertyName[3] + '=' + PropertyValue(3));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[4] + '=' + PropertyValue(4));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[5] + '=' + PropertyValue(5));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[6] + '=' + PropertyValue(6));
        FSWriteln(F, '~ ' + ParentClass.PropertyName[7] + '=' + PropertyValue(7));
    end;
    for i := 8 to ParentClass.NumProperties do
    begin
        FSWriteln(F, '~ ' + ParentClass.PropertyName[i] + '=' + PropertyValue(i));
    end;
end;

function TLineGeometryObj.NConds(): Integer;
begin
    if Freduce then
        Result := FNPhases
    else
        Result := FNConds;
end;

function TLineGeometryObj.GetYCMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;
begin
    Result := NIL;
    if dataChanged then
        UpdateLineGeometryData(f, earthModel);
    if not DSS.SolutionAbort() then
        Result := lineConstants.GetYCMatrix(f, Lngth, Units);
end;

function TLineGeometryObj.GetZMatrix(f, Lngth: Double; Units: Integer; earthModel: Integer): Tcmatrix;
begin
    Result := NIL;
    if dataChanged then
        UpdateLineGeometryData(f, earthModel);
    if not DSS.SolutionAbort() then
        Result := lineConstants.GetZMatrix(F, Lngth, Units, earthModel);
end;

procedure TLineGeometryObj.SaveWrite(F: TStream);
// Override standard SaveWrite
// Linegeometry structure not conducive to standard means of saving
var
    strPhaseChoice: String;
    iprop: Integer;
    i: Integer;
    wroteConds: Boolean = False;
begin
    // Write only properties that were explicitly set in the
    // final order they were actually set
    iProp := GetNextPropertySet(0);
    if iProp > 0 then
        FSWriteln(F);

    while iProp > 0 do
    begin
        case iProp of
            ord(TProp.cond), ord(TProp.spacing), ord(TProp.Wires):
                if not wroteConds then
                begin   // if cond=, spacing, or wires were ever used write out arrays ...
                    for i := 1 to FNConds do
                    begin
                        if conductors[i] = NIL then
                            continue; // shouldn't happen in normal conditions
                        if conductors[i].ParentClass = DSS.TSDataClass then
                            strPhaseChoice := 'tscable'
                        else if conductors[i].ParentClass = DSS.CNDataClass then
                            strPhaseChoice := 'cncable'
                        else
                            strPhaseChoice := 'wire';
                        FSWriteln(F, Format('~ Cond=%d %s=%s X=%.7g h=%.7g units=%s',
                            [i, strPhaseChoice, conductors[i].Name(), xCoord[i], yCoord[i], LineUnitsStr(units[i])]));
                    end;
                    wroteConds := True;
                end;
            ord(TProp.reduce):
                if FReduce then
                    FSWriteln(F, '~ Reduce=Yes');
            ord(TProp.Wire), ord(TProp.x), ord(TProp.h), ord(TProp.units),
            ord(TProp.CNCable), ord(TProp.TSCable):
                ; // Ignore these properties;
        else
            FSWriteln(F, Format('~ %s=%s', [ParentClass.PropertyName[iProp], CheckForBlanks(PropertyValue(iProp))]));
        end;
        iProp := GetNextPropertySet(iProp);
    end;
end;

procedure TLineGeometryObj.SetActiveCond(const Value: Integer);
begin
    if Value > 0 then
        if Value <= FNConds then
        begin
            FActiveCond := Value;
            if units[FactiveCond] = -1 then
                units[FactiveCond] := FLastUnit;  // makes this a sticky value so you don't have to repeat it
        end;
end;

procedure TLineGeometryObj.ChangeLineConstantsType(newPhaseChoice: ConductorChoice);
var
    newLineConstants: TLineConstants;
    needNew: Boolean;
begin
    newLineConstants := NIL;
    needNew := FALSE;

    if (FActiveCond > 0) and (FActiveCond <= FNConds) and 
       (newPhaseChoice <> phaseChoice[FActiveCond]) then
        needNew := TRUE
    else
    if (lineConstants = NIL) or (FNConds <> lineConstants.numConductors) then
        needNew := TRUE;

    if needNew then
        case newPhaseChoice of
            Overhead:
                newLineConstants := TOHLineConstants.Create(FNConds);
            ConcentricNeutral,
            TapeShield:
                newLineConstants := TCableConstants.Create(FNConds);
        end;

    if Assigned(newLineConstants) then
    begin
        if Assigned(lineConstants) then
        begin
            newLineConstants.Nphases := lineConstants.Nphases;
            newLineConstants.SetRhoEarth(lineConstants.FrhoEarth);
            newLineConstants.SetEpsRMedium(lineConstants.GetEpsRMedium());
        end;
        FreeAndNil(lineConstants);
        lineConstants := newLineConstants;
    end;
    if (FActiveCond > 0) and (FActiveCond <= FNConds) then
        phaseChoice[FActiveCond] := newPhaseChoice;
end;

procedure TLineGeometryObj.SetNConds(const Value: Integer);
var
    prev: Integer;
begin
    prev := FNConds;
    FNConds := Value;
    PropertySideEffects(ord(TProp.NConds), prev, [])
end;

procedure TLineGeometryObj.SetNPhases(const Value: Integer);
begin
    // TODO: remove/comment this block if using only neutrals is acceptable
    if Value < 1 then
    begin
        DoSimpleMsg(_('Invalid number of phases sent via DSS command. Please enter a value within range.'), 186);
        Exit;
    end;

    FNPhases := Value;
    lineConstants.Nphases := Value;
end;

procedure TLineGeometryObj.UpdateLineGeometryData(f: Double; earthModel: Integer);
var
    i: Integer;
    LineGeomErrMsg: String;
    cnd: TCNDataObj;
    tsd: TTSDataObj;
    cableconsts: TCableConstants;
    anyConductor: Boolean;
begin
    lineConstants.SetEquivalentSpacing(equivalentSpacing);
    if equivalentSpacing then
    begin
        // These are stored in meters in the LineConstants class, so we convert here
        lineConstants.eqDistPhPh := eqDistPhPh * To_Meters(FLastUnit);
        lineConstants.eqDistPhN := eqDistPhN * To_Meters(FLastUnit);
        lineConstants.avgPhaseHeight := avgPhaseHeight * To_Meters(FLastUnit) + lineConstants.heightOffset;
        lineConstants.avgNeutralHeight := avgNeutralHeight * To_Meters(FLastUnit) + lineConstants.heightOffset;
    end;

    anyConductor := false;
    for i := 1 to FNConds do
    begin
        if conductors[i] <> NIL then
        begin
            anyConductor := true;
            break
        end;
    end;
    if not anyConductor then
    begin
        raise Exception.Create(Format(_('%s: conductors are not correctly initialized (at least one conductor is required). Check the object definition.'), [FullName()]));
    end;

    for i := 1 to FNConds do
    begin
        if not equivalentSpacing then
        begin
            lineConstants.SetX(i, units[i], xCoord[i]);
            lineConstants.SetY(i, units[i], yCoord[i]);
        end;
        lineConstants.SetRadius(i, conductors[i].radiusUnits, conductors[i].Radius);
        lineConstants.SetCapRadius(i, conductors[i].radiusUnits, conductors[i].capRadius);
        lineConstants.SetGMR(i, conductors[i].GMRUnits, conductors[i].GMRAC);
        lineConstants.SetRdc(i, conductors[i].resistanceUnits, conductors[i].RDC);
        lineConstants.SetRac(i, conductors[i].resistanceUnits, conductors[i].RAC);

        //TODO: does it make more sense for the cable constants obj to copy the data?
        if (conductors[i] is TCNDataObj) then
        begin
            cableconsts := (lineConstants as TCableConstants);
            cnd := (conductors[i] as TCNDataObj);
            cableconsts.SetCondType(i, TConductorType.CN);
            cableconsts.SetEpsR(i, cnd.EpsR);
            cableconsts.SetInsLayer(i, cnd.radiusUnits, cnd.insLayer);
            cableconsts.SetDiaIns(i, cnd.radiusUnits, cnd.diaIns);
            cableconsts.SetDiaCable(i, cnd.radiusUnits, cnd.diaCable);
            cableconsts.SetkStrand(i, cnd.kStrand);
            cableconsts.SetDiaStrand(i, cnd.radiusUnits, cnd.DiaStrand);
            cableconsts.SetGmrStrand(i, cnd.GMRUnits, cnd.GmrStrand);
            cableconsts.SetRStrand(i, cnd.resistanceUnits, cnd.RStrand);
            cableconsts.SetSemiconLayer(i, cnd.semiconLayer);
        end
        else
        if (conductors[i] is TTSDataObj) then
        begin
            cableconsts := (lineConstants as TCableConstants);
            tsd := (conductors[i] as TTSDataObj);
            cableconsts.SetCondType(i, TConductorType.TS);
            cableconsts.SetEpsR(i, tsd.EpsR);
            cableconsts.SetInsLayer(i, tsd.radiusUnits, tsd.insLayer);
            cableconsts.SetDiaIns(i, tsd.radiusUnits, tsd.diaIns);
            cableconsts.SetDiaCable(i, tsd.radiusUnits, tsd.diaCable);
            cableconsts.SetDiaShield(i, tsd.radiusUnits, tsd.DiaShield);
            cableconsts.SetTapeLayer(i, tsd.radiusUnits, tsd.TapeLayer);
            cableconsts.SetTapeLap(i, tsd.TapeLap);
        end;
    end;

    lineConstants.Nphases := FNPhases;
    dataChanged := FALSE;

    // Before we calc, check for bad conductor definitions
    if lineConstants.ConductorsInSameSpace(LineGeomErrMsg) then
    begin
        raise ELineGeometryProblem.Create(Format(_('Error in %s: %s'), [FullName(), LineGeomErrMsg]));
        DSS.SetSolutionAbort(true);
    end
    else
    begin
        lineConstants.Calc(f, earthModel); // ***** Line impedance calc'd here ****
        if FReduce then
            lineConstants.Reduce(); // reduce out neutrals
    end;
end;

procedure TLineGeometryObj.LoadSpacingAndWires(Spc: TLineSpacingObj; Wires: pConductorDataArray; earthModel: Integer);
var
    i: Integer;
    j, actualNConds, actualNPhases: Integer;    
    newPhaseChoice: ConductorChoice;
begin
    // check the actual number of existing positions with conductors before allocating
    actualNConds := 0;
    actualNPhases := 0;
    for i := 1 to Spc.NConds do
    begin
        if Wires[i] = nil then
        begin
            continue;
        end;

        actualNConds += 1;
        if i <= Spc.Nphases then
        begin
            actualNPhases += 1;
        end;
    end;

    SetNConds(actualNConds);   // allocates
    FNPhases := actualNPhases;
    lineSpacingObj := Spc;
    if FNConds > FNPhases then
        FReduce := TRUE;

    newPhaseChoice := Overhead;
    for i := 1 to Spc.NConds do
    begin
        if Wires[i] = nil then
            continue;
        if Wires[i] is TCNDataObj then
            newPhaseChoice := ConcentricNeutral;
        if Wires[i] is TTSDataObj then
            newPhaseChoice := TapeShield;
    end;
    ChangeLineConstantsType(newPhaseChoice);

    equivalentSpacing := Spc.EquivalentSpacing();
    if equivalentSpacing then
    begin
        eqDistPhPh := Spc.eqDistPhPh;
        eqDistPhN := Spc.eqDistPhN;
        avgPhaseHeight := Spc.avgPhaseHeight;
        avgNeutralHeight := Spc.avgNeutralHeight;
        FLastUnit := Spc.Units;
    end;

    j := 0;
    for i := 1 to Spc.NConds do
    begin
        if Wires[i] = nil then
            continue;

        j += 1;
        conductors[j] := Wires[i];
        if not equivalentSpacing then
        begin
            xCoord[j] := Spc.GetXCoord(i);
            yCoord[j] := Spc.GetYCoord(i);
            units[j] := Spc.Units;
        end;
        if ((Wires[i].NormAmps < NormAmps) or (NormAmps = 0)) and (j <= FNPhases) then
        begin
            NormAmps := Wires[i].NormAmps;
            EmergAmps := Wires[i].EmergAmps;
        end;
    end;

    dataChanged := true;

    // UpdateLineGeometryData will be called when we get the impedance matrix for the line.
    // No need to call it one here because this function has already set DataChanged and also
    // LoadSpacingAndWires is only ever called from TLineObj.makeZFromSpacing which retrieves Z
    // after calling it.
end;

end.